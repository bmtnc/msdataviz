#' Prepare Data for Price Report
#'
#' Loads artifacts (with caching) and prepares data with sector and industry base rates.
#'
#' @param ticker Character string for the ticker symbol
#' @param start_date Start date for filtering (default: 2017-12-31)
#' @param end_date End date for filtering (default: NULL, no upper bound)
#' @param artifacts Optional pre-loaded artifacts list (from get_cached_artifacts)
#' @param cache_dir Directory for cache files (default: ~/.cache/msdataviz)
#' @param max_cache_age_days Maximum cache age before refresh (default: 1)
#' @param s3_bucket S3 bucket name
#' @param aws_region AWS region
#' @return List with: ticker_data, ticker, sector_name, industry_name, n_sector_stocks, n_industry_stocks
#' @export
prepare_price_report_data <- function(
    ticker,
    start_date = as.Date("2017-12-31"),
    end_date = NULL,
    artifacts = NULL,
    cache_dir = "~/.cache/msdataviz",
    max_cache_age_days = 1,
    s3_bucket = Sys.getenv("S3_BUCKET", "avpipeline-artifacts-prod"),
    aws_region = Sys.getenv("AWS_REGION", "us-east-1")
) {
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  if (is.null(artifacts)) {
    artifacts <- get_cached_artifacts(
      cache_dir = cache_dir,
      max_age_days = max_cache_age_days,
      s3_bucket = s3_bucket,
      aws_region = aws_region
    )
  }

  price_data <- artifacts$price_data
  ttm_data <- artifacts$ttm_data

  # Get sector, subsector, and industry info
  sector_name <- get_ticker_sector(ticker, ttm_data)
  subsector_name <- get_ticker_subsector(ticker, ttm_data)
  industry_name <- get_ticker_industry(ticker, ttm_data)
  sector_tickers <- get_sector_tickers(sector_name, ttm_data)
  subsector_tickers <- get_subsector_tickers(subsector_name, ttm_data)
  industry_tickers <- get_industry_tickers(industry_name, ttm_data)

  # Filter price data to date range
  filtered_prices <- price_data %>%
    dplyr::filter(date >= start_date)

  if (!is.null(end_date)) {
    filtered_prices <- filtered_prices %>%
      dplyr::filter(date <= end_date)
  }

  # Calculate sector index
  sector_data <- calculate_sector_index(
    price_data = filtered_prices,
    sector_tickers = sector_tickers
  )

  # Calculate subsector index
  subsector_data <- calculate_sector_index(
    price_data = filtered_prices,
    sector_tickers = subsector_tickers
  )

  # Calculate industry index
  industry_data <- calculate_sector_index(
    price_data = filtered_prices,
    sector_tickers = industry_tickers
  )

  # Prepare ticker data
  ticker_data <- filtered_prices %>%
    dplyr::filter(ticker == !!ticker) %>%
    dplyr::filter(!is.na(adjusted_close)) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      price = adjusted_close,
      cumulative_return = cumulative_return(price),
      drawdown = drawdown_from_high(price)
    ) %>%
    dplyr::select(date, price, cumulative_return, drawdown)

  if (nrow(ticker_data) == 0) {
    stop("No data returned for ticker '", ticker, "'")
  }

  # Join sector, subsector, and industry data to ticker data
  ticker_data <- ticker_data %>%
    dplyr::left_join(
      sector_data %>%
        dplyr::select(date, sector_cumulative_return, sector_drawdown),
      by = "date"
    ) %>%
    dplyr::left_join(
      subsector_data %>%
        dplyr::select(
          date,
          subsector_cumulative_return = sector_cumulative_return,
          subsector_drawdown = sector_drawdown
        ),
      by = "date"
    ) %>%
    dplyr::left_join(
      industry_data %>%
        dplyr::select(
          date,
          industry_cumulative_return = sector_cumulative_return,
          industry_drawdown = sector_drawdown
        ),
      by = "date"
    )

  # Calculate median current drawdown across sector, subsector, and industry stocks
  latest_date <- max(filtered_prices$date, na.rm = TRUE)

  sector_current_drawdowns <- calculate_current_drawdowns(
    filtered_prices,
    sector_tickers,
    latest_date
  )
  subsector_current_drawdowns <- calculate_current_drawdowns(
    filtered_prices,
    subsector_tickers,
    latest_date
  )
  industry_current_drawdowns <- calculate_current_drawdowns(
    filtered_prices,
    industry_tickers,
    latest_date
  )

  sector_median_drawdown <- median(sector_current_drawdowns, na.rm = TRUE)
  subsector_median_drawdown <- median(subsector_current_drawdowns, na.rm = TRUE)
  industry_median_drawdown <- median(industry_current_drawdowns, na.rm = TRUE)

  list(
    ticker_data = ticker_data,
    ticker = ticker,
    sector_name = sector_name,
    subsector_name = subsector_name,
    industry_name = industry_name,
    n_sector_stocks = length(sector_tickers),
    n_subsector_stocks = length(subsector_tickers),
    n_industry_stocks = length(industry_tickers),
    sector_median_drawdown = sector_median_drawdown,
    subsector_median_drawdown = subsector_median_drawdown,
    industry_median_drawdown = industry_median_drawdown
  )
}

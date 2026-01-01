#' Prepare Drawdown Anomaly Data
#'
#' Prepares cross-sectional data for drawdown anomaly detection.
#' Calculates current drawdown and TTM return for all tickers in sector.
#'
#' @param ticker Target ticker symbol
#' @param artifacts Optional pre-loaded artifacts list
#' @param cache_dir Directory for cache files
#' @param max_cache_age_days Maximum cache age before refresh
#' @param s3_bucket S3 bucket name
#' @param aws_region AWS region
#' @return List with: data (data frame), ticker, sector_name, industry_name
#' @export
prepare_drawdown_anomaly_data <- function(
    ticker,
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

  # Get sector and industry for target ticker
  sector_name <- get_ticker_sector(ticker, ttm_data)
  industry_name <- get_ticker_industry(ticker, ttm_data)
  sector_tickers <- get_sector_tickers(sector_name, ttm_data)

  # Get latest price and calculate cumulative high for each ticker
  latest_date <- max(price_data$date)
  one_year_ago <- latest_date - 365

  # Calculate current drawdown and TTM return for each ticker
  drawdown_data <- price_data %>%
    dplyr::filter(ticker %in% sector_tickers) %>%
    dplyr::filter(!is.na(adjusted_close)) %>%
    dplyr::arrange(ticker, date) %>%
    dplyr::group_by(ticker) %>%
    dplyr::mutate(
      cumulative_max = cummax(adjusted_close),
      drawdown = (adjusted_close / cumulative_max) - 1
    ) %>%
    dplyr::ungroup()

  # Get latest drawdown for each ticker
  latest_drawdown <- drawdown_data %>%
    dplyr::group_by(ticker) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(ticker, latest_drawdown = drawdown, latest_price = adjusted_close)

  # Get price from 1 year ago for each ticker
  year_ago_prices <- price_data %>%
    dplyr::filter(ticker %in% sector_tickers) %>%
    dplyr::filter(date <= one_year_ago) %>%
    dplyr::group_by(ticker) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(ticker, price_1y = adjusted_close)

  # Get industry for each ticker
  ticker_industries <- ttm_data %>%
    dplyr::filter(ticker %in% sector_tickers) %>%
    dplyr::group_by(ticker) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(ticker, industry)

  # Join and calculate TTM return
  anomaly_data <- latest_drawdown %>%
    dplyr::inner_join(year_ago_prices, by = "ticker") %>%
    dplyr::inner_join(ticker_industries, by = "ticker") %>%
    dplyr::mutate(
      ttm_return = (latest_price / price_1y) - 1,
      # Express drawdown as positive percentage for easier interpretation
      drawdown_pct = latest_drawdown * 100
    ) %>%
    dplyr::filter(!is.na(ttm_return) & is.finite(ttm_return)) %>%
    dplyr::select(ticker, industry, drawdown_pct, ttm_return)

  list(
    data = anomaly_data,
    ticker = ticker,
    sector_name = sector_name,
    industry_name = industry_name
  )
}

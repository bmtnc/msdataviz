#' Prepare Price Decomposition Data
#'
#' Prepares data for price decomposition chart using NOPAT per share.
#'
#' @param ticker Character string for the ticker symbol
#' @param start_date Start date for filtering (default: 2017-12-31)
#' @param end_date End date for filtering (default: NULL)
#' @param artifacts Optional pre-loaded artifacts list
#' @param cache_dir Directory for cache files
#' @param max_cache_age_days Maximum cache age before refresh
#' @param s3_bucket S3 bucket name
#' @param aws_region AWS region
#'
#' @return List with: decomposition_data, ticker, metric_display_name, base_date
#' @export
prepare_price_decomposition_data <- function(
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

  # Filter prices to date range

  ticker_prices <- price_data %>%
    dplyr::filter(ticker == !!ticker, date >= start_date) %>%
    dplyr::select(date, price = adjusted_close) %>%
    dplyr::arrange(date)

  if (!is.null(end_date)) {
    ticker_prices <- ticker_prices %>%
      dplyr::filter(date <= end_date)
  }

  # Calculate NOPAT per share from components (following prepare_valuation_data pattern)
  ticker_ttm <- ttm_data %>%
    dplyr::filter(ticker == !!ticker) %>%
    dplyr::select(
      fiscalDateEnding,
      commonStockSharesOutstanding,
      ebit_ttm,
      depreciationAndAmortization_ttm,
      depreciation_ttm
    ) %>%
    dplyr::mutate(
      ebit_per_share = ebit_ttm / commonStockSharesOutstanding,
      dep_amort_per_share = depreciationAndAmortization_ttm / commonStockSharesOutstanding,
      depreciation_per_share = depreciation_ttm / commonStockSharesOutstanding,
      fundamental_per_share = avpipeline:::calculate_nopat_per_share(
        ebit_per_share,
        dep_amort_per_share,
        depreciation_per_share
      )
    ) %>%
    dplyr::select(
      date = fiscalDateEnding,
      fundamental_per_share,
      shares_outstanding = commonStockSharesOutstanding
    ) %>%
    dplyr::arrange(date)

  if (nrow(ticker_ttm) == 0) {
    return(list(
      decomposition_data = NULL,
      ticker = ticker,
      metric_display_name = "NOPAT",
      base_date = NULL
    ))
  }

  # Join and forward-fill quarterly data to daily
  decomposition_input <- ticker_prices %>%
    dplyr::left_join(ticker_ttm, by = "date") %>%
    tidyr::fill(fundamental_per_share, shares_outstanding, .direction = "down") %>%
    dplyr::filter(
      !is.na(fundamental_per_share) &
        !is.na(shares_outstanding) &
        fundamental_per_share > 0 &
        shares_outstanding > 0
    )

  if (nrow(decomposition_input) == 0) {
    return(list(
      decomposition_data = NULL,
      ticker = ticker,
      metric_display_name = "NOPAT",
      base_date = NULL
    ))
  }

  base_date <- min(decomposition_input$date)

  decomposition_data <- calculate_price_decomposition(
    decomposition_input,
    base_date = base_date
  )

  list(
    decomposition_data = decomposition_data,
    ticker = ticker,
    metric_display_name = "NOPAT",
    base_date = base_date
  )
}

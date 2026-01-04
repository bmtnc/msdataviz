#' Prepare Price Decomposition Data
#'
#' Prepares data for price decomposition chart.
#'
#' @param ticker Character string for the ticker symbol
#' @param start_date Start date for filtering (default: 2017-12-31)
#' @param end_date End date for filtering (default: NULL)
#' @param metric Short metric name (default: "nopat"). See get_metric_config() for options.
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
    metric = "nopat",
    artifacts = NULL,
    cache_dir = "~/.cache/msdataviz",
    max_cache_age_days = 1,
    s3_bucket = Sys.getenv("S3_BUCKET", "avpipeline-artifacts-prod"),
    aws_region = Sys.getenv("AWS_REGION", "us-east-1")
) {
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  metric_config <- get_metric_config(metric)

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

  ticker_prices <- price_data %>%
    dplyr::filter(ticker == !!ticker, date >= start_date) %>%
    dplyr::select(date, price = adjusted_close) %>%
    dplyr::arrange(date)

  if (!is.null(end_date)) {
    ticker_prices <- ticker_prices %>%
      dplyr::filter(date <= end_date)
  }

  required_ttm_cols <- c(
    "fiscalDateEnding",
    "commonStockSharesOutstanding",
    metric_config$ttm_columns
  )

  ticker_ttm <- ttm_data %>%
    dplyr::filter(ticker == !!ticker) %>%
    dplyr::select(dplyr::all_of(required_ttm_cols)) %>%
    dplyr::mutate(
      fundamental_per_share = calculate_fundamental_per_share(., metric_config),
      shares_outstanding = commonStockSharesOutstanding
    ) %>%
    dplyr::select(
      date = fiscalDateEnding,
      fundamental_per_share,
      shares_outstanding
    ) %>%
    dplyr::arrange(date)

  if (nrow(ticker_ttm) == 0) {
    return(list(
      decomposition_data = NULL,
      ticker = ticker,
      metric_display_name = metric_config$display_name,
      base_date = NULL
    ))
  }

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
      metric_display_name = metric_config$display_name,
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
    metric_display_name = metric_config$display_name,
    base_date = base_date
  )
}

#' Prepare Per-Share Economics Data
#'
#' Prepares data for per-share economics visualization showing share count effects.
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
#' @return List with: data, ticker, metric_display_name, base_date
#' @export
prepare_per_share_economics_data <- function(
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

  ttm_data <- artifacts$ttm_data

  required_ttm_cols <- c(
    "fiscalDateEnding",
    "commonStockSharesOutstanding",
    metric_config$ttm_columns
  )

  ticker_ttm <- ttm_data %>%
    dplyr::filter(ticker == !!ticker) %>%
    dplyr::select(dplyr::all_of(required_ttm_cols)) %>%
    dplyr::arrange(fiscalDateEnding)

  if (nrow(ticker_ttm) == 0) {
    return(list(
      data = NULL,
      ticker = ticker,
      metric_display_name = metric_config$display_name,
      base_date = NULL
    ))
  }

  ticker_ttm <- ticker_ttm %>%
    dplyr::mutate(
      date = fiscalDateEnding,
      shares_outstanding = commonStockSharesOutstanding,
      fundamental_per_share = calculate_fundamental_per_share(., metric_config),
      total_fundamental = fundamental_per_share * shares_outstanding
    ) %>%
    dplyr::filter(
      date >= start_date &
        !is.na(fundamental_per_share) &
        !is.na(shares_outstanding) &
        shares_outstanding > 0
    )

  if (!is.null(end_date)) {
    ticker_ttm <- ticker_ttm %>%
      dplyr::filter(date <= end_date)
  }

  if (nrow(ticker_ttm) == 0) {
    return(list(
      data = NULL,
      ticker = ticker,
      metric_display_name = metric_config$display_name,
      base_date = NULL
    ))
  }

  base_date <- min(ticker_ttm$date)
  base_row <- ticker_ttm %>%
    dplyr::filter(date == base_date) %>%
    dplyr::slice(1)

  base_total <- base_row$total_fundamental
  base_per_share <- base_row$fundamental_per_share
  base_shares <- base_row$shares_outstanding

  per_share_data <- ticker_ttm %>%
    dplyr::mutate(
      total_fundamental_indexed = total_fundamental / base_total - 1,
      per_share_indexed = fundamental_per_share / base_per_share - 1,
      shares_indexed = shares_outstanding / base_shares - 1,
      share_count_contribution = total_fundamental_indexed - per_share_indexed
    ) %>%
    dplyr::select(
      date, shares_outstanding, fundamental_per_share, total_fundamental,
      total_fundamental_indexed, per_share_indexed, shares_indexed, share_count_contribution
    )

  list(
    data = per_share_data,
    ticker = ticker,
    metric_display_name = metric_config$display_name,
    base_date = base_date
  )
}

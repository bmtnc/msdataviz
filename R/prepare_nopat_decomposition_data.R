#' Prepare NOPAT Decomposition Data
#'
#' Prepares data for NOPAT decomposition chart from TTM artifact.
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
#' @return List with: nopat_decomposition_data, ticker, base_date
#' @export
prepare_nopat_decomposition_data <- function(
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

  ttm_data <- artifacts$ttm_data

  tax_rate <- 0.2375

  ticker_data <- ttm_data %>%
    dplyr::filter(ticker == !!ticker) %>%
    dplyr::mutate(
      date = fiscalDateEnding,
      amortization = dplyr::coalesce(depreciationAndAmortization_ttm, 0) -
        dplyr::coalesce(depreciation_ttm, 0),
      nopat = (ebit_ttm + pmax(amortization, 0)) * (1 - tax_rate),
      ic = dplyr::coalesce(totalShareholderEquity, 0) +
        dplyr::coalesce(shortLongTermDebtTotal, 0) +
        dplyr::coalesce(capitalLeaseObligations, 0)
    ) %>%
    dplyr::filter(date >= start_date) %>%
    dplyr::arrange(date)

  if (!is.null(end_date)) {
    ticker_data <- ticker_data %>%
      dplyr::filter(date <= end_date)
  }

  ticker_data <- ticker_data %>%
    dplyr::filter(!is.na(nopat) & !is.na(ic) & ic > 0) %>%
    dplyr::mutate(roic = nopat / ic) %>%
    dplyr::filter(is.finite(roic)) %>%
    dplyr::select(date, ic, roic)

  if (nrow(ticker_data) < 2) {
    return(list(
      nopat_decomposition_data = NULL,
      ticker = ticker,
      base_date = NULL
    ))
  }

  base_date <- min(ticker_data$date)

  nopat_decomposition_data <- calculate_nopat_decomposition(
    ticker_data,
    base_date = base_date
  )

  list(
    nopat_decomposition_data = nopat_decomposition_data,
    ticker = ticker,
    base_date = base_date
  )
}

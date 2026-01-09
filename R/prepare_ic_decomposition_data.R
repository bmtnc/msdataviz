#' Prepare Invested Capital Decomposition Data
#'
#' Prepares data for IC decomposition chart from TTM artifact.
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
#' @return List with: ic_decomposition_data, ticker, base_date
#' @export
prepare_ic_decomposition_data <- function(
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

  ticker_data <- ttm_data %>%
    dplyr::filter(ticker == !!ticker) %>%
    dplyr::mutate(date = fiscalDateEnding) %>%
    dplyr::filter(date >= start_date) %>%
    dplyr::arrange(date)

  if (!is.null(end_date)) {
    ticker_data <- ticker_data %>%
      dplyr::filter(date <= end_date)
  }

  if (nrow(ticker_data) < 2) {
    return(list(
      ic_decomposition_data = NULL,
      ticker = ticker,
      base_date = NULL
    ))
  }

  ic_input <- ticker_data %>%
    dplyr::mutate(
      net_income = dplyr::coalesce(`netIncome.cf`, 0),
      dividends = dplyr::coalesce(dividendPayout, 0),
      debt = dplyr::coalesce(shortLongTermDebtTotal, 0) +
        dplyr::coalesce(capitalLeaseObligations, 0),
      equity = dplyr::coalesce(totalShareholderEquity, 0)
    ) %>%
    dplyr::select(date, net_income, dividends, debt, equity) %>%
    dplyr::filter(!is.na(date))

  base_date <- min(ic_input$date)

  ic_decomposition_data <- calculate_ic_decomposition(ic_input, base_date = base_date)

  list(
    ic_decomposition_data = ic_decomposition_data,
    ticker = ticker,
    base_date = base_date
  )
}

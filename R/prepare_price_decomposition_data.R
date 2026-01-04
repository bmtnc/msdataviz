#' Prepare Price Decomposition Data
#'
#' Prepares data for price decomposition chart.
#'
#' @param ticker Character string for the ticker symbol
#' @param start_date Start date for filtering (default: 2017-12-31)
#' @param end_date End date for filtering (default: NULL)
#' @param numerator Valuation numerator: "price" or "ev" (default: "price")
#' @param metric Short metric name (default: "nopat"). See get_metric_config() for options.
#' @param artifacts Optional pre-loaded artifacts list
#' @param cache_dir Directory for cache files
#' @param max_cache_age_days Maximum cache age before refresh
#' @param s3_bucket S3 bucket name
#' @param aws_region AWS region
#'
#' @return List with: decomposition_data, ticker, metric_display_name, numerator, base_date
#' @export
prepare_price_decomposition_data <- function(
    ticker,
    start_date = as.Date("2017-12-31"),
    end_date = NULL,
    numerator = "price",
    metric = "nopat",
    artifacts = NULL,
    cache_dir = "~/.cache/msdataviz",
    max_cache_age_days = 1,
    s3_bucket = Sys.getenv("S3_BUCKET", "avpipeline-artifacts-prod"),
    aws_region = Sys.getenv("AWS_REGION", "us-east-1")
) {
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")
  avpipeline::validate_character_scalar(numerator, allow_empty = FALSE, name = "numerator")

  if (!numerator %in% c("price", "ev")) {
    stop("numerator must be 'price' or 'ev', got: '", numerator, "'")
  }

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

  if (numerator == "ev") {
    ev_cols <- c(
      "shortLongTermDebtTotal",
      "capitalLeaseObligations",
      "cashAndShortTermInvestments",
      "longTermInvestments"
    )
    required_ttm_cols <- c(required_ttm_cols, ev_cols)
  }

  ticker_ttm <- ttm_data %>%
    dplyr::filter(ticker == !!ticker) %>%
    dplyr::select(dplyr::all_of(required_ttm_cols))

  ticker_ttm <- ticker_ttm %>%
    dplyr::mutate(
      fundamental_per_share = calculate_fundamental_per_share(., metric_config),
      shares_outstanding = commonStockSharesOutstanding
    )

  if (numerator == "ev") {
    ticker_ttm <- ticker_ttm %>%
      dplyr::mutate(
        debt_per_share = shortLongTermDebtTotal / commonStockSharesOutstanding,
        lease_per_share = capitalLeaseObligations / commonStockSharesOutstanding,
        cash_per_share = cashAndShortTermInvestments / commonStockSharesOutstanding,
        lt_invest_per_share = longTermInvestments / commonStockSharesOutstanding
      )
  }

  select_cols <- c("date", "fundamental_per_share", "shares_outstanding")
  if (numerator == "ev") {
    select_cols <- c(select_cols, "debt_per_share", "lease_per_share",
                     "cash_per_share", "lt_invest_per_share")
  }

  ticker_ttm <- ticker_ttm %>%
    dplyr::mutate(date = fiscalDateEnding) %>%
    dplyr::select(dplyr::all_of(select_cols)) %>%
    dplyr::arrange(date)

  if (nrow(ticker_ttm) == 0) {
    return(list(
      decomposition_data = NULL,
      ticker = ticker,
      metric_display_name = metric_config$display_name,
      numerator = numerator,
      base_date = NULL
    ))
  }

  decomposition_input <- ticker_prices %>%
    dplyr::left_join(ticker_ttm, by = "date")

  if (numerator == "ev") {
    decomposition_input <- decomposition_input %>%
      tidyr::fill(
        fundamental_per_share, shares_outstanding,
        debt_per_share, lease_per_share, cash_per_share, lt_invest_per_share,
        .direction = "down"
      ) %>%
      dplyr::mutate(
        price = avpipeline:::calculate_enterprise_value_per_share(
          price,
          debt_per_share,
          lease_per_share,
          cash_per_share,
          lt_invest_per_share
        )
      )
  } else {
    decomposition_input <- decomposition_input %>%
      tidyr::fill(fundamental_per_share, shares_outstanding, .direction = "down")
  }

  decomposition_input <- decomposition_input %>%
    dplyr::filter(
      !is.na(fundamental_per_share) &
        !is.na(shares_outstanding) &
        !is.na(price) &
        fundamental_per_share > 0 &
        shares_outstanding > 0
    )

  if (nrow(decomposition_input) == 0) {
    return(list(
      decomposition_data = NULL,
      ticker = ticker,
      metric_display_name = metric_config$display_name,
      numerator = numerator,
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
    numerator = numerator,
    base_date = base_date
  )
}

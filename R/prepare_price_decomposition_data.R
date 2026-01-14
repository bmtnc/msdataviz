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
#' @return List with: decomposition_data, kpi_data, share_count_decomposition_data,
#'   tsr_decomposition_data, ticker, metric_display_name, numerator, base_date
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
      fundamental_total = calculate_fundamental_total(., metric_config)
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

  select_cols <- c("date", "fundamental_per_share", "fundamental_total", "commonStockSharesOutstanding")
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
      kpi_data = NULL,
      share_count_decomposition_data = NULL,
      tsr_decomposition_data = NULL,
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
        fundamental_per_share,
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
      tidyr::fill(fundamental_per_share, .direction = "down")
  }

  decomposition_input <- decomposition_input %>%
    dplyr::filter(
      !is.na(fundamental_per_share) &
        !is.na(price) &
        fundamental_per_share > 0
    )

  if (nrow(decomposition_input) == 0) {
    return(list(
      decomposition_data = NULL,
      kpi_data = NULL,
      share_count_decomposition_data = NULL,
      tsr_decomposition_data = NULL,
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

  # Extract quarterly KPI data (before daily fill-forward)
  kpi_data <- ticker_ttm %>%
    dplyr::select(date, value = fundamental_per_share) %>%
    dplyr::filter(!is.na(value), date >= start_date)

  if (!is.null(end_date)) {
    kpi_data <- kpi_data %>%
      dplyr::filter(date <= end_date)
  }

  # Prepare share count decomposition data
  share_count_input <- ticker_ttm %>%
    dplyr::select(
      date,
      metric = fundamental_total,
      shares = commonStockSharesOutstanding
    ) %>%
    dplyr::filter(
      !is.na(metric),
      !is.na(shares),
      metric > 0,
      shares > 0,
      date >= start_date
    )

  if (!is.null(end_date)) {
    share_count_input <- share_count_input %>%
      dplyr::filter(date <= end_date)
  }

  share_count_decomposition_data <- if (nrow(share_count_input) > 1) {
    calculate_share_count_decomposition(share_count_input, base_date = base_date)
  } else {
    NULL
  }

  # Prepare TSR decomposition data
  # Requires close and split_coefficient columns for 3-way decomposition
  has_close_col <- "close" %in% names(price_data)
  has_split_col <- "split_coefficient" %in% names(price_data)

  tsr_decomposition_data <- NULL

 if (has_close_col && has_split_col) {
    ticker_prices_for_tsr <- price_data %>%
      dplyr::filter(ticker == !!ticker, date >= start_date) %>%
      dplyr::select(date, adjusted_close, close, split_coefficient) %>%
      dplyr::filter(
        !is.na(adjusted_close), adjusted_close > 0,
        !is.na(close), close > 0
      ) %>%
      dplyr::arrange(date)

    if (!is.null(end_date)) {
      ticker_prices_for_tsr <- ticker_prices_for_tsr %>%
        dplyr::filter(date <= end_date)
    }

    quarterly_shares <- ticker_ttm %>%
      dplyr::select(date, shares = commonStockSharesOutstanding) %>%
      dplyr::filter(!is.na(shares), shares > 0) %>%
      dplyr::arrange(date)

    # Join quarterly shares onto daily prices and forward-fill
    tsr_input <- ticker_prices_for_tsr %>%
      dplyr::left_join(quarterly_shares, by = "date") %>%
      tidyr::fill(shares, .direction = "down") %>%
      dplyr::filter(!is.na(shares))

    tsr_decomposition_data <- if (nrow(tsr_input) > 1) {
      calculate_tsr_decomposition(tsr_input, base_date = base_date)
    } else {
      NULL
    }
  } else {
    message("TSR decomposition skipped: price artifact missing 'close' or 'split_coefficient' columns. ",
            "Regenerate artifact with updated pipeline to enable 3-way decomposition.")
  }

  list(
    decomposition_data = decomposition_data,
    kpi_data = kpi_data,
    share_count_decomposition_data = share_count_decomposition_data,
    tsr_decomposition_data = tsr_decomposition_data,
    ticker = ticker,
    metric_display_name = metric_config$display_name,
    numerator = numerator,
    base_date = base_date
  )
}

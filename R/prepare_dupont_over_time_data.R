#' Prepare DuPont Over Time Data
#'
#' Prepares quarterly ROE and ROA data for a ticker.
#'
#' @param ticker Character string for the ticker symbol
#' @param start_date Start date for filtering (default: 2017-12-31)
#' @param end_date End date for filtering (default: NULL)
#' @param income_metric Income metric: "nopat", "netIncome", "ebit", "ebitda", "operatingIncome" (default: "nopat")
#' @param artifacts Optional pre-loaded artifacts list
#' @param cache_dir Directory for cache files
#' @param max_cache_age_days Maximum cache age before refresh
#' @param s3_bucket S3 bucket name
#' @param aws_region AWS region
#' @return List with: data (data frame with date, roe, roa), ticker, income_metric_name
#' @export
prepare_dupont_over_time_data <- function(
    ticker,
    start_date = as.Date("2017-12-31"),
    end_date = NULL,
    income_metric = "nopat",
    artifacts = NULL,
    cache_dir = "~/.cache/msdataviz",
    max_cache_age_days = 1,
    s3_bucket = Sys.getenv("S3_BUCKET", "avpipeline-artifacts-prod"),
    aws_region = Sys.getenv("AWS_REGION", "us-east-1")
) {
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")
  avpipeline::validate_character_scalar(income_metric, allow_empty = FALSE, name = "income_metric")

  if (is.null(artifacts)) {
    artifacts <- get_cached_artifacts(
      cache_dir = cache_dir,
      max_age_days = max_cache_age_days,
      s3_bucket = s3_bucket,
      aws_region = aws_region
    )
  }

  ttm_data <- artifacts$ttm_data

  # Map metric names to columns and display names
  metric_config <- list(
    nopat = list(
      cols = c("ebit_ttm", "depreciationAndAmortization_ttm", "depreciation_ttm"),
      display_name = "NOPAT",
      is_calculated = TRUE
    ),
    netIncome = list(
      cols = "netIncome_ttm",
      display_name = "Net Income",
      is_calculated = FALSE
    ),
    ebit = list(
      cols = "ebit_ttm",
      display_name = "EBIT",
      is_calculated = FALSE
    ),
    ebitda = list(
      cols = "ebitda_ttm",
      display_name = "EBITDA",
      is_calculated = FALSE
    ),
    operatingIncome = list(
      cols = "operatingIncome_ttm",
      display_name = "Operating Income",
      is_calculated = FALSE
    )
  )

  if (!income_metric %in% names(metric_config)) {
    stop(
      "Unknown income_metric: '", income_metric, "'. ",
      "Available: ", paste(names(metric_config), collapse = ", ")
    )
  }

  config <- metric_config[[income_metric]]
  required_cols <- c("fiscalDateEnding", "totalAssets", "totalShareholderEquity", config$cols)
  missing_cols <- setdiff(required_cols, names(ttm_data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in ttm_data: ", paste(missing_cols, collapse = ", "))
  }

  # Filter to ticker first
  ticker_data <- ttm_data %>%
    dplyr::filter(ticker == !!ticker)


  # Calculate income based on metric type
  if (config$is_calculated && income_metric == "nopat") {
    tax_rate <- 0.2375
    ticker_data <- ticker_data %>%
      dplyr::mutate(
        amortization = dplyr::coalesce(depreciationAndAmortization_ttm, 0) -
          dplyr::coalesce(depreciation_ttm, 0),
        income = (ebit_ttm + pmax(amortization, 0)) * (1 - tax_rate)
      )
  } else {
    col_name <- config$cols
    ticker_data <- ticker_data %>%
      dplyr::mutate(income = .data[[col_name]])
  }

  dupont_data <- ticker_data %>%
    dplyr::select(
      date = fiscalDateEnding,
      income,
      assets = totalAssets,
      equity = totalShareholderEquity
    ) %>%
    dplyr::filter(date >= start_date) %>%
    {
      if (!is.null(end_date)) dplyr::filter(., date <= end_date) else .
    } %>%
    dplyr::filter(
      !is.na(income) & !is.na(assets) & !is.na(equity) &
      assets > 0 & equity > 0
    ) %>%
    dplyr::mutate(
      roe = calculate_roe(income, equity),
      roa = calculate_roa(income, assets)
    ) %>%
    dplyr::filter(is.finite(roe) & is.finite(roa)) %>%
    dplyr::select(date, roe, roa) %>%
    dplyr::arrange(date)

  list(
    data = dupont_data,
    ticker = ticker,
    income_metric = income_metric,
    income_metric_name = config$display_name
  )
}

#' Prepare DuPont Over Time Data
#'
#' Prepares quarterly return metric and ROA data for a ticker.
#'
#' @param ticker Character string for the ticker symbol
#' @param start_date Start date for filtering (default: 2017-12-31)
#' @param end_date End date for filtering (default: NULL)
#' @param numerator Income metric: "nopat", "netIncome", "ebit", "ebitda", "operatingIncome", "grossProfit" (default: "nopat")
#' @param denominator Capital base: "equity", "invested_capital", or "total_assets" (default: "equity")
#' @param artifacts Optional pre-loaded artifacts list
#' @param cache_dir Directory for cache files
#' @param max_cache_age_days Maximum cache age before refresh
#' @param s3_bucket S3 bucket name
#' @param aws_region AWS region
#' @return List with: data (data frame with date, return_metric, roa), ticker, numerator_name, denominator, mode
#' @export
prepare_dupont_over_time_data <- function(
    ticker,
    start_date = as.Date("2017-12-31"),
    end_date = NULL,
    numerator = "nopat",
    denominator = "equity",
    artifacts = NULL,
    cache_dir = "~/.cache/msdataviz",
    max_cache_age_days = 1,
    s3_bucket = Sys.getenv("S3_BUCKET", "avpipeline-artifacts-prod"),
    aws_region = Sys.getenv("AWS_REGION", "us-east-1")
) {
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")
  avpipeline::validate_character_scalar(numerator, allow_empty = FALSE, name = "numerator")
  avpipeline::validate_character_scalar(denominator, allow_empty = FALSE, name = "denominator")

  if (!denominator %in% c("equity", "invested_capital", "total_assets")) {
    stop("denominator must be 'equity', 'invested_capital', or 'total_assets'")
  }

  if (is.null(artifacts)) {
    artifacts <- get_cached_artifacts(
      cache_dir = cache_dir,
      max_age_days = max_cache_age_days,
      s3_bucket = s3_bucket,
      aws_region = aws_region
    )
  }

  ttm_data <- artifacts$ttm_data

  # Map numerator names to columns and display names
  numerator_config <- list(
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
    ),
    grossProfit = list(
      cols = "grossProfit_ttm",
      display_name = "Gross Profit",
      is_calculated = FALSE
    )
  )

  if (!numerator %in% names(numerator_config)) {
    stop(
      "Unknown numerator: '", numerator, "'. ",
      "Available: ", paste(names(numerator_config), collapse = ", ")
    )
  }

  config <- numerator_config[[numerator]]

  # Build required columns based on denominator
  base_cols <- c("fiscalDateEnding", "totalAssets", config$cols)
  if (denominator == "equity") {
    base_cols <- c(base_cols, "totalShareholderEquity")
  } else if (denominator == "invested_capital") {
    base_cols <- c(base_cols, "totalShareholderEquity", "shortLongTermDebtTotal", "capitalLeaseObligations")
  }
  # total_assets: no additional columns needed beyond totalAssets

  missing_cols <- setdiff(base_cols, names(ttm_data))
  if (length(missing_cols) > 0) {
    stop("Missing columns in ttm_data: ", paste(missing_cols, collapse = ", "))
  }

  # Filter to ticker

  ticker_data <- ttm_data %>%
    dplyr::filter(ticker == !!ticker)

  # Calculate income based on numerator type
  if (config$is_calculated && numerator == "nopat") {
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

  # Build base data
  if (denominator == "equity") {
    dupont_data <- ticker_data %>%
      dplyr::select(
        date = fiscalDateEnding,
        income,
        assets = totalAssets,
        denom = totalShareholderEquity
      )
  } else if (denominator == "invested_capital") {
    dupont_data <- ticker_data %>%
      dplyr::mutate(
        invested_capital = totalShareholderEquity +
          dplyr::coalesce(shortLongTermDebtTotal, 0) +
          dplyr::coalesce(capitalLeaseObligations, 0)
      ) %>%
      dplyr::select(
        date = fiscalDateEnding,
        income,
        assets = totalAssets,
        denom = invested_capital
      )
  } else {
    # total_assets: denom = assets (ROA decomposition where return = roa)
    dupont_data <- ticker_data %>%
      dplyr::select(
        date = fiscalDateEnding,
        income,
        assets = totalAssets,
        denom = totalAssets
      )
  }

  dupont_data <- dupont_data %>%
    dplyr::filter(date >= start_date) %>%
    {
      if (!is.null(end_date)) dplyr::filter(., date <= end_date) else .
    } %>%
    dplyr::filter(
      !is.na(income) & !is.na(assets) & !is.na(denom) &
      assets > 0 & denom > 0
    ) %>%
    dplyr::mutate(
      return_metric = income / denom,
      roa = income / assets
    ) %>%
    dplyr::filter(is.finite(return_metric) & is.finite(roa)) %>%
    dplyr::select(date, return_metric, roa) %>%
    dplyr::arrange(date)

  mode <- switch(
    denominator,
    equity = "roe",
    invested_capital = "roic",
    total_assets = "roa"
  )

  list(
    data = dupont_data,
    ticker = ticker,
    numerator_name = config$display_name,
    denominator = denominator,
    mode = mode
  )
}

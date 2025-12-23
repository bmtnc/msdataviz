# =============================================================================
# Single Stock Explorer - Process On-The-Fly
# =============================================================================
# Processes a single stock through the TTM pipeline and renders visualizations.
# Can read from S3 (if raw data exists) or fetch fresh from API.
#
# Usage:
#   1. Set TICKER and other config parameters below
#   2. Run the script
# =============================================================================

# ---- CONFIGURATION PARAMETERS -----------------------------------------------
TICKER <- "LULU"

# Data source: "s3" (read from S3) or "api" (fetch fresh from Alpha Vantage)
DATA_SOURCE <- "s3"

# S3 configuration (only needed if DATA_SOURCE = "s3")
S3_BUCKET <- Sys.getenv("S3_BUCKET", "avpipeline-artifacts-prod")
AWS_REGION <- Sys.getenv("AWS_REGION", "us-east-1")

# Pipeline parameters
START_DATE <- as.Date("2004-12-31")
THRESHOLD <- 4
LOOKBACK <- 5
LOOKAHEAD <- 5
END_WINDOW_SIZE <- 5
END_THRESHOLD <- 3
MIN_OBS <- 10
DELAY_SECONDS <- 1

# Fundamental KPI (quarterly bar plot)
FUNDAMENTAL_METRIC <- "nopat_ttm_per_share"
# FUNDAMENTAL_METRIC <- "fcf_ttm_per_share"
# FUNDAMENTAL_METRIC <- "ebitda_ttm_per_share"
# FUNDAMENTAL_METRIC <- "grossProfit_ttm_per_share"
# FUNDAMENTAL_METRIC <- "tangible_book_value_per_share"
# FUNDAMENTAL_METRIC <- "operatingCashflow_ttm_per_share"

# Valuation metric (daily line plot with callout)
VALUATION_METRIC <- "ev_nopat"
# VALUATION_METRIC <- "ev_ebitda"
# VALUATION_METRIC <- "ev_fcf"
# VALUATION_METRIC <- "ev_gp"
# VALUATION_METRIC <- "roic"
# VALUATION_METRIC <- "market_cap"

# ---- SECTION 1: Load required functions -------------------------------------
avpipeline::set_ggplot_theme()

# ---- SECTION 2: Fetch/Load and Process Data ---------------------------------
cat("=", paste(rep("=", 70), collapse = ""), "\n")
cat("Processing", TICKER, "from", DATA_SOURCE, "...\n")
cat("=", paste(rep("=", 70), collapse = ""), "\n\n")

if (DATA_SOURCE == "s3") {
  if (S3_BUCKET == "") {
    stop("S3_BUCKET environment variable required when DATA_SOURCE = 's3'")
  }

  cat("Reading raw data from S3...\n")
  ttm_per_share_data <- avpipeline::process_ticker_from_s3(
    ticker = TICKER,
    bucket_name = S3_BUCKET,
    start_date = START_DATE,
    region = AWS_REGION,
    threshold = THRESHOLD,
    lookback = LOOKBACK,
    lookahead = LOOKAHEAD,
    end_window_size = END_WINDOW_SIZE,
    end_threshold = END_THRESHOLD,
    min_obs = MIN_OBS
  )
} else {
  cat("Fetching fresh data from Alpha Vantage API...\n")
  result <- avpipeline::process_single_ticker(
    ticker = TICKER,
    start_date = START_DATE,
    threshold = THRESHOLD,
    lookback = LOOKBACK,
    lookahead = LOOKAHEAD,
    end_window_size = END_WINDOW_SIZE,
    end_threshold = END_THRESHOLD,
    min_obs = MIN_OBS,
    delay_seconds = DELAY_SECONDS
  )
  ttm_per_share_data <- result$data
}

if (is.null(ttm_per_share_data) || nrow(ttm_per_share_data) == 0) {
  stop("No data returned for ", TICKER)
}

cat("Processed dataset:", nrow(ttm_per_share_data), "rows,",
    ncol(ttm_per_share_data), "columns\n\n")

# ---- SECTION 3: Calculate Additional Metrics --------------------------------
cat("Calculating derived metrics...\n")

ttm_per_share_data <- ttm_per_share_data %>%
  dplyr::mutate(
    tangible_book_value_per_share = totalShareholderEquity_per_share -
      dplyr::coalesce(goodwill_per_share, 0),
    ev_ebitda = enterprise_value_per_share / ebitda_ttm_per_share,
    ev_nopat = enterprise_value_per_share / nopat_ttm_per_share,
    ev_fcf = enterprise_value_per_share / fcf_ttm_per_share,
    ev_gp = enterprise_value_per_share / grossProfit_ttm_per_share,
    roic = nopat_ttm_per_share / invested_capital_per_share * 100
  )

# ---- SECTION 4: Validate Metrics --------------------------------------------
if (!FUNDAMENTAL_METRIC %in% names(ttm_per_share_data)) {
  available <- names(ttm_per_share_data)
  stop("Fundamental metric '", FUNDAMENTAL_METRIC, "' not found.\n",
       "Available: ", paste(head(available, 20), collapse = ", "), "...")
}

if (!VALUATION_METRIC %in% names(ttm_per_share_data)) {
  available <- names(ttm_per_share_data)
  stop("Valuation metric '", VALUATION_METRIC, "' not found.\n",
       "Available: ", paste(head(available, 20), collapse = ", "), "...")
}

# ---- SECTION 5: Create Fundamental KPI Bar Plot (Quarterly) -----------------
cat("Creating fundamental KPI bar plot -", FUNDAMENTAL_METRIC, "...\n")

fundamental_data <- ttm_per_share_data %>%
  dplyr::filter(!is.na(!!rlang::sym(FUNDAMENTAL_METRIC))) %>%
  dplyr::distinct(fiscalDateEnding, .keep_all = TRUE) %>%
  dplyr::select(fiscalDateEnding, value = !!rlang::sym(FUNDAMENTAL_METRIC)) %>%
  dplyr::arrange(fiscalDateEnding) %>%
  dplyr::slice_tail(n = 54)

if (nrow(fundamental_data) == 0) {
  cat("No fundamental data available for", FUNDAMENTAL_METRIC, "\n")
} else {
  p1 <- fundamental_data %>%
    ggplot2::ggplot(ggplot2::aes(x = fiscalDateEnding, y = value)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
    ggplot2::labs(
      title = paste0(TICKER, ": ", FUNDAMENTAL_METRIC),
      x = "Fiscal Date Ending",
      y = FUNDAMENTAL_METRIC
    ) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )

  if (max(fundamental_data$value, na.rm = TRUE) > 1e9) {
    p1 <- p1 + ggplot2::scale_y_continuous(
      labels = scales::label_number(scale = 1e-9, suffix = "B")
    )
  } else if (max(fundamental_data$value, na.rm = TRUE) > 1e6) {
    p1 <- p1 + ggplot2::scale_y_continuous(
      labels = scales::label_number(scale = 1e-6, suffix = "M")
    )
  }

  print(p1)
  cat("Fundamental bar plot created with", nrow(fundamental_data), "quarters\n\n")
}

# ---- SECTION 6: Create Valuation Line Plot (Daily with Callout) -------------
cat("Creating valuation line plot -", VALUATION_METRIC, "...\n")

valuation_data <- ttm_per_share_data %>%
  dplyr::filter(!is.na(!!rlang::sym(VALUATION_METRIC))) %>%
  dplyr::filter(date >= Sys.Date() - 5000) %>%
  dplyr::select(date, value = !!rlang::sym(VALUATION_METRIC)) %>%
  dplyr::arrange(date)

if (nrow(valuation_data) == 0) {
  cat("No valuation data available for", VALUATION_METRIC, "\n")
} else {
  most_recent <- valuation_data %>%
    dplyr::slice_tail(n = 1)

  p2 <- valuation_data %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line(color = "darkgreen", alpha = 0.8, linewidth = 1) +
    ggplot2::geom_point(
      data = most_recent,
      ggplot2::aes(x = date, y = value),
      color = "black",
      size = 3
    ) +
    ggplot2::geom_label(
      data = most_recent,
      ggplot2::aes(
        x = date,
        y = value,
        label = scales::comma(value, accuracy = 0.01)
      ),
      nudge_x = as.numeric(diff(range(valuation_data$date))) * 0.04,
      nudge_y = max(valuation_data$value, na.rm = TRUE) * 0.02,
      color = "black",
      fill = "white",
      alpha = 0.8,
      size = 2.5,
      fontface = "bold"
    ) +
    ggplot2::labs(
      title = paste0(TICKER, ": ", VALUATION_METRIC),
      x = "Date",
      y = VALUATION_METRIC
    ) +
    ggplot2::coord_cartesian(
      xlim = c(
        min(valuation_data$date),
        max(valuation_data$date) + as.numeric(diff(range(valuation_data$date))) * 0.12
      )
    ) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )

  if (max(valuation_data$value, na.rm = TRUE) > 1e9) {
    p2 <- p2 + ggplot2::scale_y_continuous(
      labels = scales::label_number(scale = 1e-9, suffix = "B")
    )
  } else if (max(valuation_data$value, na.rm = TRUE) > 1e6) {
    p2 <- p2 + ggplot2::scale_y_continuous(
      labels = scales::label_number(scale = 1e-6, suffix = "M")
    )
  }

  print(p2)
  cat("Valuation line plot created with", nrow(valuation_data), "daily points\n\n")
}

# ---- SECTION 7: Summary Statistics ------------------------------------------
cat("=", paste(rep("=", 70), collapse = ""), "\n")
cat("SUMMARY\n")
cat("=", paste(rep("=", 70), collapse = ""), "\n")
cat("Ticker:", TICKER, "\n")
cat("Date range:", as.character(min(ttm_per_share_data$date)),
    "to", as.character(max(ttm_per_share_data$date)), "\n")
cat("Quarters:", length(unique(ttm_per_share_data$fiscalDateEnding)), "\n")
cat("Daily observations:", nrow(ttm_per_share_data), "\n")

latest <- ttm_per_share_data %>%
  dplyr::filter(date == max(date))

cat("\nLatest values (", as.character(latest$date[1]), "):\n", sep = "")
cat("  ", FUNDAMENTAL_METRIC, ": ",
    scales::comma(latest[[FUNDAMENTAL_METRIC]][1], accuracy = 0.01), "\n", sep = "")
cat("  ", VALUATION_METRIC, ": ",
    scales::comma(latest[[VALUATION_METRIC]][1], accuracy = 0.01), "\n", sep = "")
cat("  market_cap: ",
    scales::comma(latest$market_cap[1], accuracy = 1, scale = 1e-3, suffix = "B"), "\n", sep = "")

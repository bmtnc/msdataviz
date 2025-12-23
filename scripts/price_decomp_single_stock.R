# =============================================================================
# Stock Price Decomposition Analysis - Single Stock On-The-Fly
# =============================================================================
# Processes a single stock through the pipeline and decomposes price changes.
# Can read from S3 (if raw data exists) or fetch fresh from API.
#
# Decomposes cumulative stock price changes into:
# 1. Change in per-share fundamentals (with sub-breakdown)
# 2. Change in valuation multiple
#
# Key Formula: Price = Fundamental per share x Multiple
# =============================================================================

# ---- CONFIGURATION PARAMETERS -----------------------------------------------
TICKER <- "AMZN"

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

# Choose the fundamental metric for decomposition
FUNDAMENTAL_METRIC <- "nopat_ttm_per_share"
# FUNDAMENTAL_METRIC <- "grossProfit_ttm_per_share"
# FUNDAMENTAL_METRIC <- "fcf_ttm_per_share"
# FUNDAMENTAL_METRIC <- "ebitda_ttm_per_share"
# FUNDAMENTAL_METRIC <- "operatingCashflow_ttm_per_share"

# Analysis period (number of days back from most recent data)
ANALYSIS_DAYS <- 750
# ANALYSIS_DAYS <- 1350

# Base date for decomposition (NULL = use first available date in period)
BASE_DATE <- NULL
# BASE_DATE <- as.Date("2020-01-01")

PLOT_TITLE <- NULL

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

# ---- SECTION 3: Validate parameters -----------------------------------------
if (!FUNDAMENTAL_METRIC %in% names(ttm_per_share_data)) {
  stop("Fundamental metric '", FUNDAMENTAL_METRIC, "' not found in dataset")
}

if (!"commonStockSharesOutstanding" %in% names(ttm_per_share_data)) {
  stop("commonStockSharesOutstanding not found - required for decomposition")
}

# ---- SECTION 4: Prepare decomposition data ----------------------------------
cat("Preparing enhanced price decomposition data for", TICKER, "...\n")

price_data <- ttm_per_share_data %>%
  dplyr::filter(!is.na(adjusted_close)) %>%
  dplyr::filter(!is.na(!!rlang::sym(FUNDAMENTAL_METRIC))) %>%
  dplyr::filter(!is.na(commonStockSharesOutstanding)) %>%
  dplyr::filter(!!rlang::sym(FUNDAMENTAL_METRIC) > 0) %>%
  dplyr::filter(commonStockSharesOutstanding > 0) %>%
  dplyr::arrange(date) %>%
  dplyr::slice_tail(n = ANALYSIS_DAYS) %>%
  dplyr::select(
    date,
    price = adjusted_close,
    fundamental_per_share = !!rlang::sym(FUNDAMENTAL_METRIC),
    shares_outstanding = commonStockSharesOutstanding
  ) %>%
  dplyr::mutate(
    total_fundamental = fundamental_per_share * shares_outstanding,
    multiple = price / fundamental_per_share
  )

if (nrow(price_data) == 0) {
  stop("No valid data available for ", TICKER, " - ", FUNDAMENTAL_METRIC)
}

# Determine base date
if (is.null(BASE_DATE)) {
  base_date <- min(price_data$date)
} else {
  base_date <- BASE_DATE
  if (!base_date %in% price_data$date) {
    base_date <- price_data$date[which.min(abs(price_data$date - BASE_DATE))]
    cat("Adjusted base date to nearest available:", as.character(base_date), "\n")
  }
}

# Get base values
base_values <- price_data %>%
  dplyr::filter(date == base_date) %>%
  dplyr::slice(1)

base_price <- base_values$price
base_fundamental_per_share <- base_values$fundamental_per_share
base_shares <- base_values$shares_outstanding
base_total_fundamental <- base_values$total_fundamental
base_multiple <- base_values$multiple

cat("Base date:", as.character(base_date), "\n")
cat("Base price: $", round(base_price, 2), "\n")
cat("Base", FUNDAMENTAL_METRIC, ":", round(base_fundamental_per_share, 2), "\n")
cat("Base shares outstanding:", round(base_shares / 1e9, 2), "B\n")
cat("Base multiple:", round(base_multiple, 1), "x\n")

# ---- SECTION 5: Calculate decomposition -------------------------------------
decomposition_data <- price_data %>%
  dplyr::filter(date >= base_date) %>%
  dplyr::mutate(
    fundamental_per_share_change = fundamental_per_share - base_fundamental_per_share,
    multiple_change = multiple - base_multiple,
    price_change = price - base_price,

    fundamental_contribution = fundamental_per_share_change * base_multiple,
    multiple_contribution = base_fundamental_per_share * multiple_change,

    interaction_term = fundamental_per_share_change * multiple_change,

    fundamental_contrib_adj = fundamental_contribution +
      ifelse(abs(fundamental_contribution) + abs(multiple_contribution) > 0,
             interaction_term * abs(fundamental_contribution) /
             (abs(fundamental_contribution) + abs(multiple_contribution)),
             interaction_term / 2),

    multiple_contrib_adj = multiple_contribution +
      ifelse(abs(fundamental_contribution) + abs(multiple_contribution) > 0,
             interaction_term * abs(multiple_contribution) /
             (abs(fundamental_contribution) + abs(multiple_contribution)),
             interaction_term / 2),

    actual_per_share_change = fundamental_per_share - base_fundamental_per_share,

    nopat_growth_per_share_effect = (total_fundamental - base_total_fundamental) / shares_outstanding,
    share_count_per_share_effect = actual_per_share_change - nopat_growth_per_share_effect,

    nopat_growth_contribution = ifelse(abs(actual_per_share_change) > 1e-10,
                                      fundamental_contrib_adj * nopat_growth_per_share_effect / actual_per_share_change,
                                      fundamental_contrib_adj * 0.5),

    share_count_contribution = ifelse(abs(actual_per_share_change) > 1e-10,
                                     fundamental_contrib_adj * share_count_per_share_effect / actual_per_share_change,
                                     fundamental_contrib_adj * 0.5)
  ) %>%
  dplyr::select(
    date, price, fundamental_per_share, shares_outstanding, total_fundamental, multiple,
    price_change,
    fundamental_contribution = fundamental_contrib_adj,
    multiple_contribution = multiple_contrib_adj,
    nopat_growth_contribution,
    share_count_contribution
  )

# ---- SECTION 6: Create enhanced stacked area chart --------------------------
cat("Creating enhanced price decomposition visualization...\n")

current_data <- decomposition_data %>% dplyr::slice_tail(n = 1)

if (abs(current_data$price_change) > 0.01) {
  nopat_dollars <- current_data$nopat_growth_contribution
  share_dollars <- current_data$share_count_contribution
  valuation_dollars <- current_data$multiple_contribution

  metric_name <- tolower(gsub("_ttm_per_share", "", FUNDAMENTAL_METRIC))

  nopat_label <- paste0(
    ifelse(nopat_dollars >= 0, paste0(metric_name, " Growth"), paste0(metric_name, " Decline")),
    ": ", ifelse(nopat_dollars >= 0, "+", "-"), "$", round(abs(nopat_dollars), 1)
  )

  share_label <- paste0(
    ifelse(share_dollars >= 0, "Buybacks", "Dilution"),
    ": ", ifelse(share_dollars >= 0, "+", "-"), "$", round(abs(share_dollars), 1)
  )

  valuation_label <- paste0(
    ifelse(valuation_dollars >= 0, "Valuation Expansion", "Valuation Compression"),
    ": ", ifelse(valuation_dollars >= 0, "+", "-"), "$", round(abs(valuation_dollars), 1)
  )

  total_change <- current_data$price_change
  direction_text <- ifelse(total_change > 0, "Gain", "Decline")

  subtitle_text <- paste0(
    "$", round(abs(total_change), 1), " Cumulative ", direction_text, " Contribution:", "\n",
    nopat_label, " | ", share_label, " | ", valuation_label
  )
} else {
  subtitle_text <- "No significant price change to analyze"
}

plot_data <- decomposition_data %>%
  dplyr::select(date, nopat_growth_contribution, share_count_contribution, multiple_contribution) %>%
  tidyr::pivot_longer(
    cols = c(nopat_growth_contribution, share_count_contribution, multiple_contribution),
    names_to = "component",
    values_to = "contribution"
  ) %>%
  dplyr::mutate(
    component = dplyr::case_when(
      component == "nopat_growth_contribution" ~ paste("\u0394", gsub("_ttm_per_share", "", FUNDAMENTAL_METRIC)),
      component == "share_count_contribution" ~ "\u0394 Share Count",
      component == "multiple_contribution" ~ "\u0394 Valuation",
      TRUE ~ component
    ),
    component = factor(component, levels = c(
      paste("\u0394", gsub("_ttm_per_share", "", FUNDAMENTAL_METRIC)),
      "\u0394 Share Count",
      "\u0394 Valuation"
    ))
  )

fundamental_label <- paste("\u0394", gsub("_ttm_per_share", "", FUNDAMENTAL_METRIC))
shares_label <- "\u0394 Share Count"
multiple_label <- "\u0394 Valuation"

color_values <- c("steelblue", "lightblue", "darkgreen")
names(color_values) <- c(fundamental_label, shares_label, multiple_label)

callout_text <- paste0(
  "$", round(current_data$price, 2), "\n",
  "", round(current_data$multiple, 1), "x"
)

p <- plot_data %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = contribution, fill = component)) +
  ggplot2::geom_area(alpha = 0.7, position = "stack") +
  ggplot2::geom_line(
    data = decomposition_data,
    ggplot2::aes(x = date, y = price_change),
    color = "black",
    linewidth = 1,
    inherit.aes = FALSE
  ) +
  ggplot2::geom_point(
    data = current_data,
    ggplot2::aes(x = date, y = price_change),
    color = "black",
    size = 3,
    inherit.aes = FALSE
  ) +
  ggplot2::geom_label(
    data = current_data,
    ggplot2::aes(
      x = date,
      y = price_change,
      label = callout_text
    ),
    nudge_x = as.numeric(diff(range(decomposition_data$date))) * 0.06,
    nudge_y = max(decomposition_data$price_change, na.rm = TRUE) * 0.05,
    color = "black",
    fill = "white",
    alpha = 0.9,
    size = 3,
    fontface = "bold",
    lineheight = 0.9,
    inherit.aes = FALSE
  ) +
  ggplot2::scale_fill_manual(values = color_values) +
  ggplot2::labs(
    title = ifelse(is.null(PLOT_TITLE),
                   paste0(TICKER, ": Cumulative Price Change Decomposition"),
                   PLOT_TITLE),
    subtitle = subtitle_text,
    x = "Date",
    y = "Cumulative Price Change ($)",
    fill = "",
    caption = paste0(
      "Methodology: Price = EPS x Valuation Multiple\n",
      "Start Date: ", base_date
    )
  ) +
  ggplot2::coord_cartesian(
    xlim = c(
      min(decomposition_data$date),
      max(decomposition_data$date) + as.numeric(diff(range(decomposition_data$date))) * 0.15
    )
  ) +
  ggplot2::scale_x_date(date_breaks = "6 months", date_labels = "%Y") +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 0, hjust = 1),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = ggplot2::element_text(size = 11, lineheight = 1.2, hjust = 0),
    plot.caption = ggplot2::element_text(hjust = 1)
  )

max_change <- max(abs(decomposition_data$price_change), na.rm = TRUE)
if (max_change > 100) {
  p <- p + ggplot2::scale_y_continuous(labels = scales::dollar_format())
} else {
  p <- p + ggplot2::scale_y_continuous(labels = scales::dollar_format(accuracy = 0.01))
}

print(p)

# ---- SECTION 7: Summary statistics ------------------------------------------
cat("\n=== ENHANCED DECOMPOSITION SUMMARY ===\n")

cat("Period:", as.character(base_date), "to", as.character(current_data$date), "\n")
cat("Total price change: $", round(current_data$price_change, 2), "\n\n")

cat("Main decomposition:\n")
cat("  - Fundamental contribution: $", round(current_data$fundamental_contribution, 2),
    " (", round(100 * current_data$fundamental_contribution / current_data$price_change, 1), "%)\n")
cat("  - Multiple contribution: $", round(current_data$multiple_contribution, 2),
    " (", round(100 * current_data$multiple_contribution / current_data$price_change, 1), "%)\n\n")

cat("Fundamental breakdown:\n")
cat("  - From total", gsub("_ttm_per_share", "", FUNDAMENTAL_METRIC), "growth: $",
    round(current_data$nopat_growth_contribution, 2),
    " (", round(100 * current_data$nopat_growth_contribution / current_data$price_change, 1), "%)\n")
cat("  - From share count changes: $", round(current_data$share_count_contribution, 2),
    " (", round(100 * current_data$share_count_contribution / current_data$price_change, 1), "%)\n")

cat("\nCurrent metrics:\n")
cat("  - Price: $", round(current_data$price, 2), "\n")
cat("  -", FUNDAMENTAL_METRIC, ":", round(current_data$fundamental_per_share, 2), "\n")
cat("  - Shares outstanding:", round(current_data$shares_outstanding / 1e9, 2), "B\n")
cat("  - Multiple:", round(current_data$multiple, 1), "x\n")

share_change_pct <- (current_data$shares_outstanding - base_shares) / base_shares * 100
total_fundamental_change_pct <- (current_data$total_fundamental - base_total_fundamental) / base_total_fundamental * 100

cat("\nChange summary:\n")
cat("  - Share count change:", round(share_change_pct, 1), "%\n")
cat("  - Total", gsub("_ttm_per_share", "", FUNDAMENTAL_METRIC), "change:",
    round(total_fundamental_change_pct, 1), "%\n")
cat("\nData points:", nrow(decomposition_data), "\n")

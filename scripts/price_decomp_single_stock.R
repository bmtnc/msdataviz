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
TICKER <- "META"

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
set_ggplot_theme()

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

cat(
  "Processed dataset:",
  nrow(ttm_per_share_data),
  "rows,",
  ncol(ttm_per_share_data),
  "columns\n\n"
)

# ---- SECTION 3: Validate parameters -----------------------------------------
if (!FUNDAMENTAL_METRIC %in% names(ttm_per_share_data)) {
  stop("Fundamental metric '", FUNDAMENTAL_METRIC, "' not found in dataset")
}

if (!"commonStockSharesOutstanding" %in% names(ttm_per_share_data)) {
  stop("commonStockSharesOutstanding not found - required for decomposition")
}

# ---- SECTION 4: Prepare decomposition data ----------------------------------
cat("Preparing price decomposition data for", TICKER, "...\n")

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
    cat(
      "Adjusted base date to nearest available:",
      as.character(base_date),
      "\n"
    )
  }
}

# Get base values for logging
base_values <- price_data %>%
  dplyr::filter(date == base_date) %>%
  dplyr::slice(1) %>%
  dplyr::mutate(
    total_fundamental = fundamental_per_share * shares_outstanding,
    multiple = price / fundamental_per_share
  )

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
decomposition_data <- calculate_price_decomposition(price_data, base_date)

# ---- SECTION 6: Create enhanced stacked area chart --------------------------
cat("Creating enhanced price decomposition visualization...\n")

current_data <- decomposition_data %>% dplyr::slice_tail(n = 1)

p <- plot_price_decomposition(
  decomposition_data = decomposition_data,
  ticker = TICKER,
  metric_name = FUNDAMENTAL_METRIC,
  base_date = base_date,
  title = PLOT_TITLE
)

print(p)


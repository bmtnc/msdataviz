#' Render Fundamentals Report for a Ticker
#'
#' Renders an HTML report with bar charts for key financial metrics.
#'
#' @param ticker Character string for the ticker symbol
#' @param output_dir Directory for output file (default: "output")
#' @param start_date Start date for filtering data (default: 2014-12-31)
#' @param end_date End date for filtering data (default: NULL)
#' @param s3_bucket S3 bucket name
#' @param aws_region AWS region
#'
#' @return Path to the rendered HTML file
#' @export
render_fundamentals_report <- function(
    ticker,
    output_dir = "output",
    start_date = as.Date("2014-12-31"),
    end_date = NULL,
    s3_bucket = Sys.getenv("S3_BUCKET", "avpipeline-artifacts-prod"),
    aws_region = Sys.getenv("AWS_REGION", "us-east-1")
) {
  artifacts <- get_cached_artifacts(
    s3_bucket = s3_bucket,
    aws_region = aws_region
  )

  fundamentals <- prepare_fundamentals_data(
    ticker = ticker,
    ttm_data = artifacts$ttm_data,
    start_date = start_date,
    end_date = end_date
  )

  if (nrow(fundamentals) == 0) {
    stop("No data found for ticker: ", ticker)
  }

  # Prepare TTM data with calculated metrics for per-share decomposition
  ttm_with_calcs <- artifacts$ttm_data %>%
    dplyr::filter(ticker == !!ticker) %>%
    dplyr::mutate(
      nopat = calculate_nopat(
        ebit_ttm,
        depreciationAndAmortization_ttm,
        depreciation_ttm
      ),
      fcf = calculate_fcf(operatingCashflow_ttm, capitalExpenditures_ttm)
    )

  # Prepare TSR decomposition data
  tsr_data <- NULL
  base_date <- NULL

  price_data <- artifacts$price_data
  has_close <- "close" %in% names(price_data)
  has_split <- "split_coefficient" %in% names(price_data)

  if (has_close && has_split) {
    ticker_prices <- price_data %>%
      dplyr::filter(ticker == !!ticker, date >= start_date) %>%
      dplyr::select(date, adjusted_close, close, split_coefficient) %>%
      dplyr::filter(
        !is.na(adjusted_close), adjusted_close > 0,
        !is.na(close), close > 0
      ) %>%
      dplyr::arrange(date)

    if (!is.null(end_date)) {
      ticker_prices <- ticker_prices %>%
        dplyr::filter(date <= end_date)
    }

    quarterly_shares <- ttm_with_calcs %>%
      dplyr::select(date = fiscalDateEnding, shares = commonStockSharesOutstanding) %>%
      dplyr::filter(!is.na(shares), shares > 0) %>%
      dplyr::arrange(date)

    tsr_input <- ticker_prices %>%
      dplyr::left_join(quarterly_shares, by = "date") %>%
      tidyr::fill(shares, .direction = "down") %>%
      dplyr::filter(!is.na(shares))

    if (nrow(tsr_input) > 1) {
      base_date <- min(tsr_input$date)
      tsr_data <- calculate_tsr_decomposition(tsr_input, base_date = base_date)
    }
  }

  # Prepare drawdown data
  drawdown_data <- price_data %>%
    dplyr::filter(ticker == !!ticker, date >= start_date) %>%
    {
      if (!is.null(end_date)) dplyr::filter(., date <= end_date) else .
    } %>%
    dplyr::arrange(date) %>%
    dplyr::select(date, price = adjusted_close) %>%
    dplyr::filter(!is.na(price), price > 0) %>%
    dplyr::mutate(drawdown = drawdown_from_high(price))

  # Prepare per-share decomposition data for each metric
  per_share_revenue <- prepare_per_share_decomposition_data(
    ttm_with_calcs, ticker, "totalRevenue_ttm", start_date, end_date
  )
  per_share_gross_profit <- prepare_per_share_decomposition_data(
    ttm_with_calcs, ticker, "grossProfit_ttm", start_date, end_date
  )
  per_share_ebit <- prepare_per_share_decomposition_data(
    ttm_with_calcs, ticker, "ebit_ttm", start_date, end_date
  )
  per_share_ebitda <- prepare_per_share_decomposition_data(
    ttm_with_calcs, ticker, "ebitda_ttm", start_date, end_date
  )
  per_share_nopat <- prepare_per_share_decomposition_data(
    ttm_with_calcs, ticker, "nopat", start_date, end_date
  )
  per_share_fcf <- prepare_per_share_decomposition_data(
    ttm_with_calcs, ticker, "fcf", start_date, end_date
  )
  per_share_cfo <- prepare_per_share_decomposition_data(
    ttm_with_calcs, ticker, "operatingCashflow_ttm", start_date, end_date
  )
  per_share_bv <- prepare_per_share_decomposition_data(
    ttm_with_calcs, ticker, "totalShareholderEquity", start_date, end_date
  )

  # Prepare ROIC decomposition data
  roic_result <- prepare_dupont_over_time_data(
    ticker = ticker,
    start_date = start_date,
    end_date = end_date,
    numerator = "nopat",
    denominator = "invested_capital",
    artifacts = artifacts
  )
  roic_labels <- get_dupont_labels("nopat", "invested_capital")

  # Prepare ROE decomposition data
  roe_result <- prepare_dupont_over_time_data(
    ticker = ticker,
    start_date = start_date,
    end_date = end_date,
    numerator = "netIncome",
    denominator = "equity",
    artifacts = artifacts
  )
  roe_labels <- get_dupont_labels("netIncome", "equity")

  # Prepare KPI data for financial ratios
  kpi_data <- prepare_kpi_data(fundamentals)

  # Prepare valuation multiples data (daily frequency)
  valuation_data <- prepare_valuation_multiples_data(
    ticker = ticker,
    price_data = price_data,
    ttm_data = artifacts$ttm_data,
    start_date = start_date,
    end_date = end_date
  )

  template_path <- system.file(
    "templates", "fundamentals_report.Rmd",
    package = "msdataviz"
  )

  if (template_path == "") {
    stop("Template not found. Is msdataviz installed?")
  }

  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  output_file <- file.path(output_dir, paste0(ticker, "_fundamentals_report.html"))

  rmarkdown::render(
    input = template_path,
    output_file = output_file,
    params = list(
      ticker = ticker,
      data = fundamentals,
      tsr_data = tsr_data,
      base_date = base_date,
      drawdown_data = drawdown_data,
      per_share_revenue = per_share_revenue,
      per_share_gross_profit = per_share_gross_profit,
      per_share_ebit = per_share_ebit,
      per_share_ebitda = per_share_ebitda,
      per_share_nopat = per_share_nopat,
      per_share_fcf = per_share_fcf,
      per_share_cfo = per_share_cfo,
      per_share_bv = per_share_bv,
      roic_data = roic_result$data,
      roic_labels = roic_labels,
      roe_data = roe_result$data,
      roe_labels = roe_labels,
      kpi_data = kpi_data,
      valuation_data = valuation_data
    ),
    quiet = TRUE
  )

  output_file
}

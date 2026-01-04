#' Render Price Report for a Ticker
#'
#' Fetches data from S3 and renders an HTML price report with sector and industry base rates.
#'
#' @param ticker Character string for the ticker symbol
#' @param output_dir Directory for output file (default: current directory)
#' @param start_date Start date for filtering data (default: 2017-12-31)
#' @param end_date End date for filtering data (default: NULL, no upper bound)
#' @param decomposition_numerator Numerator for decomposition: "price" or "ev" (default: "price")
#' @param decomposition_metric Metric for price decomposition (default: "nopat")
#' @param s3_bucket S3 bucket name
#' @param aws_region AWS region
#'
#' @return Path to the rendered HTML file
#' @export
render_price_report <- function(
    ticker,
    output_dir = "output",
    start_date = as.Date("2017-12-31"),
    end_date = NULL,
    decomposition_numerator = "price",
    decomposition_metric = "nopat",
    s3_bucket = Sys.getenv("S3_BUCKET", "avpipeline-artifacts-prod"),
    aws_region = Sys.getenv("AWS_REGION", "us-east-1")
) {
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  # Load artifacts once for all data preps
  artifacts <- get_cached_artifacts(
    s3_bucket = s3_bucket,
    aws_region = aws_region
  )

  report_data <- prepare_price_report_data(
    ticker = ticker,
    start_date = start_date,
    end_date = end_date,
    artifacts = artifacts,
    s3_bucket = s3_bucket,
    aws_region = aws_region
  )

  valuation_result <- prepare_valuation_data(
    ticker = ticker,
    start_date = start_date,
    end_date = end_date,
    artifacts = artifacts,
    s3_bucket = s3_bucket,
    aws_region = aws_region
  )

  anomaly_result <- prepare_valuation_anomaly_data(
    ticker = ticker,
    artifacts = artifacts,
    s3_bucket = s3_bucket,
    aws_region = aws_region
  )

  momentum_anomaly_result <- prepare_momentum_anomaly_data(
    ticker = ticker,
    artifacts = artifacts,
    s3_bucket = s3_bucket,
    aws_region = aws_region
  )

  dupont_anomaly_result <- prepare_dupont_anomaly_data(
    ticker = ticker,
    artifacts = artifacts,
    s3_bucket = s3_bucket,
    aws_region = aws_region
  )

  roic_result <- prepare_roic_data(
    ticker = ticker,
    start_date = start_date,
    end_date = end_date,
    artifacts = artifacts,
    s3_bucket = s3_bucket,
    aws_region = aws_region
  )

  decomposition_result <- prepare_price_decomposition_data(
    ticker = ticker,
    start_date = start_date,
    end_date = end_date,
    numerator = decomposition_numerator,
    metric = decomposition_metric,
    artifacts = artifacts,
    s3_bucket = s3_bucket,
    aws_region = aws_region
  )

  # Prepare rolling beta data
  ttm_data <- artifacts$ttm_data
  sector_name <- get_ticker_sector(ticker, ttm_data)
  sector_tickers <- get_sector_tickers(sector_name, ttm_data)

  filtered_prices <- artifacts$price_data %>%
    dplyr::filter(date >= start_date)

  if (!is.null(end_date)) {
    filtered_prices <- filtered_prices %>%
      dplyr::filter(date <= end_date)
  }

  rolling_beta_data <- prepare_rolling_beta_data(
    price_data = filtered_prices,
    target_ticker = ticker,
    sector_tickers = sector_tickers,
    roll_window = 252L
  )

  template_path <- system.file(
    "templates", "price_report.Rmd",
    package = "msdataviz"
  )

  if (template_path == "") {
    stop("Template not found. Is msdataviz installed?")
  }

  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  output_file <- file.path(output_dir, paste0(ticker, "_price_report.html"))

  rmarkdown::render(
    input = template_path,
    output_file = output_file,
    params = list(
      ticker = report_data$ticker,
      ticker_data = report_data$ticker_data,
      sector_name = report_data$sector_name,
      subsector_name = report_data$subsector_name,
      industry_name = report_data$industry_name,
      n_sector_stocks = report_data$n_sector_stocks,
      n_subsector_stocks = report_data$n_subsector_stocks,
      n_industry_stocks = report_data$n_industry_stocks,
      sector_median_drawdown = report_data$sector_median_drawdown,
      subsector_median_drawdown = report_data$subsector_median_drawdown,
      industry_median_drawdown = report_data$industry_median_drawdown,
      valuation_data = valuation_result$valuation_data,
      valuation_metric_name = valuation_result$metric_name,
      valuation_n_sector_stocks = valuation_result$n_sector_stocks,
      valuation_n_subsector_stocks = valuation_result$n_subsector_stocks,
      valuation_n_industry_stocks = valuation_result$n_industry_stocks,
      roic_data = roic_result$roic_data,
      roic_n_sector_stocks = roic_result$n_sector_stocks,
      roic_n_subsector_stocks = roic_result$n_subsector_stocks,
      roic_n_industry_stocks = roic_result$n_industry_stocks,
      anomaly_data = anomaly_result$data,
      momentum_anomaly_data = momentum_anomaly_result$data,
      dupont_anomaly_data = dupont_anomaly_result$data,
      rolling_beta_data = rolling_beta_data,
      decomposition_data = decomposition_result$decomposition_data,
      decomposition_metric_display_name = decomposition_result$metric_display_name,
      decomposition_numerator = decomposition_result$numerator,
      decomposition_base_date = decomposition_result$base_date
    ),
    quiet = TRUE
  )

  output_file
}

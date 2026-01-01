#' Render Price Report for a Ticker
#'
#' Fetches data from S3 and renders an HTML price report with sector and industry base rates.
#'
#' @param ticker Character string for the ticker symbol
#' @param output_dir Directory for output file (default: current directory)
#' @param start_date Start date for filtering data (default: 2017-12-31)
#' @param end_date End date for filtering data (default: NULL, no upper bound)
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

  drawdown_anomaly_result <- prepare_drawdown_anomaly_data(
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
      industry_name = report_data$industry_name,
      n_sector_stocks = report_data$n_sector_stocks,
      n_industry_stocks = report_data$n_industry_stocks,
      sector_median_drawdown = report_data$sector_median_drawdown,
      industry_median_drawdown = report_data$industry_median_drawdown,
      valuation_data = valuation_result$valuation_data,
      valuation_metric_name = valuation_result$metric_name,
      valuation_n_sector_stocks = valuation_result$n_sector_stocks,
      valuation_n_industry_stocks = valuation_result$n_industry_stocks,
      roic_data = roic_result$roic_data,
      roic_n_sector_stocks = roic_result$n_sector_stocks,
      roic_n_industry_stocks = roic_result$n_industry_stocks,
      anomaly_data = anomaly_result$data,
      drawdown_anomaly_data = drawdown_anomaly_result$data
    ),
    quiet = TRUE
  )

  output_file
}

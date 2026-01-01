#' Render Price Report for a Ticker
#'
#' Fetches data from S3 and renders an HTML price report with reference group base rates.
#'
#' @param ticker Character string for the ticker symbol
#' @param reference_level Reference group level: "sector" or "industry" (default: "sector")
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
    reference_level = c("sector", "industry"),
    output_dir = "output",
    start_date = as.Date("2017-12-31"),
    end_date = NULL,
    s3_bucket = Sys.getenv("S3_BUCKET", "avpipeline-artifacts-prod"),
    aws_region = Sys.getenv("AWS_REGION", "us-east-1")
) {
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")
  reference_level <- match.arg(reference_level)

  # Load artifacts once for all data preps
  artifacts <- get_cached_artifacts(
    s3_bucket = s3_bucket,
    aws_region = aws_region
  )

  report_data <- prepare_price_report_data(
    ticker = ticker,
    reference_level = reference_level,
    start_date = start_date,
    end_date = end_date,
    artifacts = artifacts,
    s3_bucket = s3_bucket,
    aws_region = aws_region
  )

  valuation_result <- prepare_valuation_data(
    ticker = ticker,
    reference_level = reference_level,
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
      reference_name = report_data$reference_name,
      n_reference_stocks = report_data$n_reference_stocks,
      valuation_data = valuation_result$valuation_data,
      valuation_metric_name = valuation_result$metric_name,
      valuation_n_stocks = valuation_result$n_reference_stocks,
      anomaly_data = anomaly_result$data,
      sector_name = anomaly_result$sector_name,
      industry_name = anomaly_result$industry_name
    ),
    quiet = TRUE
  )

  output_file
}

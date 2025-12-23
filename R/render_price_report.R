#' Render Price Report for a Ticker
#'
#' Fetches data from S3 and renders an HTML price report.
#'
#' @param ticker Character string for the ticker symbol
#' @param output_dir Directory for output file (default: current directory)
#' @param s3_bucket S3 bucket name
#' @param aws_region AWS region
#' @param start_date Start date for data
#'
#' @return Path to the rendered HTML file
#' @export
render_price_report <- function(
    ticker,
    output_dir = ".",
    s3_bucket = Sys.getenv("S3_BUCKET", "avpipeline-artifacts-prod"),
    aws_region = Sys.getenv("AWS_REGION", "us-east-1"),
    start_date = as.Date("2004-12-31")
) {
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  price_data <- avpipeline::process_ticker_from_s3(
    ticker = ticker,
    bucket_name = s3_bucket,
    start_date = start_date,
    region = aws_region
  )

  if (is.null(price_data) || nrow(price_data) == 0) {
    stop("No data returned for ", ticker)
  }

  price_data <- price_data %>%
    dplyr::filter(!is.na(adjusted_close)) %>%
    dplyr::select(date, price = adjusted_close) %>%
    dplyr::arrange(date)

  template_path <- system.file(
    "templates", "price_report.Rmd",
    package = "msdataviz"
  )

  if (template_path == "") {
    stop("Template not found. Is msdataviz installed?")
  }

  output_file <- file.path(output_dir, paste0(ticker, "_price_report.html"))

  rmarkdown::render(
    input = template_path,
    output_file = output_file,
    params = list(
      ticker = ticker,
      price_data = price_data
    ),
    quiet = TRUE
  )

  output_file
}

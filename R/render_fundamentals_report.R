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
      per_share_revenue = per_share_revenue,
      per_share_gross_profit = per_share_gross_profit,
      per_share_ebit = per_share_ebit,
      per_share_ebitda = per_share_ebitda,
      per_share_nopat = per_share_nopat,
      per_share_fcf = per_share_fcf,
      per_share_cfo = per_share_cfo,
      per_share_bv = per_share_bv
    ),
    quiet = TRUE
  )

  output_file
}

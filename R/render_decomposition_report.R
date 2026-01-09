#' Render Decomposition Report for a Ticker
#'
#' Fetches data from S3 and renders an HTML decomposition report with three exhibits:
#' price decomposition, per-share KPI growth, and DuPont return decomposition.
#'
#' @param ticker Character string for the ticker symbol
#' @param output_dir Directory for output file (default: current directory)
#' @param start_date Start date for filtering data (default: 2017-12-31)
#' @param end_date End date for filtering data (default: NULL, no upper bound)
#' @param profile Configuration profile: "default", "financial", "asset_heavy", "early_stage"
#' @param price_decomp_metric Override metric for price decomposition
#' @param price_decomp_numerator Override numerator: "price" or "ev"
#' @param kpi_metric Override metric for KPI chart
#' @param dupont_numerator Override numerator for DuPont
#' @param dupont_denominator Override denominator: "equity", "invested_capital", "total_assets"
#' @param s3_bucket S3 bucket name
#' @param aws_region AWS region
#'
#' @return Path to the rendered HTML file
#' @export
render_decomposition_report <- function(
    ticker,
    output_dir = "output",
    start_date = as.Date("2014-12-31"),
    end_date = NULL,
    profile = "default",
    price_decomp_metric = NULL,
    price_decomp_numerator = NULL,
    kpi_metric = NULL,
    dupont_numerator = NULL,
    dupont_denominator = NULL,
    s3_bucket = Sys.getenv("S3_BUCKET", "avpipeline-artifacts-prod"),
    aws_region = Sys.getenv("AWS_REGION", "us-east-1")
) {
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  # Resolve configuration from profile + overrides
  config <- resolve_report_config(
    profile = profile,
    price_decomp_metric = price_decomp_metric,
    price_decomp_numerator = price_decomp_numerator,
    kpi_metric = kpi_metric,
    dupont_numerator = dupont_numerator,
    dupont_denominator = dupont_denominator
  )

  # Get display configs
  price_metric_config <- get_metric_config(config$price_decomp_metric)
  kpi_metric_config <- get_metric_config(config$kpi_metric)
  dupont_num_config <- get_metric_config(config$dupont_numerator)
  dupont_denom_config <- get_denominator_config(config$dupont_denominator)
  dupont_labels <- get_dupont_labels(config$dupont_numerator, config$dupont_denominator)

  # Load artifacts once
  artifacts <- get_cached_artifacts(
    s3_bucket = s3_bucket,
    aws_region = aws_region
  )

  # Prepare price decomposition data
  decomposition_result <- prepare_price_decomposition_data(
    ticker = ticker,
    start_date = start_date,
    end_date = end_date,
    numerator = config$price_decomp_numerator,
    metric = config$price_decomp_metric,
    artifacts = artifacts,
    s3_bucket = s3_bucket,
    aws_region = aws_region
  )

  # Prepare DuPont data
  dupont_result <- prepare_dupont_over_time_data(
    ticker = ticker,
    start_date = start_date,
    end_date = end_date,
    numerator = config$dupont_numerator,
    denominator = config$dupont_denominator,
    artifacts = artifacts,
    s3_bucket = s3_bucket,
    aws_region = aws_region
  )

  # Prepare IC decomposition data
  ic_result <- prepare_ic_decomposition_data(
    ticker = ticker,
    start_date = start_date,
    end_date = end_date,
    artifacts = artifacts,
    s3_bucket = s3_bucket,
    aws_region = aws_region
  )

  # Find template
  template_path <- system.file(
    "templates", "decomposition_report.Rmd",
    package = "msdataviz"
  )

  if (template_path == "") {
    stop("Template not found. Is msdataviz installed?")
  }

  # Create output directory
  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  output_file <- file.path(output_dir, paste0(ticker, "_decomposition_report.html"))

  # Render report
  rmarkdown::render(
    input = template_path,
    output_file = output_file,
    params = list(
      ticker = ticker,
      # Price decomposition
      decomposition_data = decomposition_result$decomposition_data,
      decomposition_metric_display_name = price_metric_config$display_name,
      decomposition_numerator = config$price_decomp_numerator,
      decomposition_base_date = decomposition_result$base_date,
      # TSR decomposition
      tsr_decomposition_data = decomposition_result$tsr_decomposition_data,
      # KPI per share
      share_count_decomposition_data = decomposition_result$share_count_decomposition_data,
      kpi_metric_display_name = kpi_metric_config$display_name,
      # DuPont
      dupont_data = dupont_result$data,
      dupont_numerator_name = dupont_num_config$display_name,
      dupont_denominator_name = dupont_denom_config$display_name,
      dupont_return_label = dupont_labels$return_label,
      dupont_roa_label = dupont_labels$roa_label,
      dupont_effect_label = dupont_labels$effect_label,
      dupont_multiplier_label = dupont_labels$multiplier_label,
      dupont_title_suffix = dupont_labels$title_suffix,
      dupont_footnote = dupont_labels$footnote,
      # IC decomposition
      ic_decomposition_data = ic_result$ic_decomposition_data
    ),
    quiet = TRUE
  )

  output_file
}

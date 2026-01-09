#' Resolve Report Configuration
#'
#' Merges profile defaults with explicit overrides.
#'
#' @param profile Profile name (default: "default")
#' @param price_decomp_metric Override for price decomposition metric
#' @param price_decomp_numerator Override for price decomposition numerator
#' @param kpi_metric Override for KPI chart metric
#' @param dupont_numerator Override for DuPont numerator
#' @param dupont_denominator Override for DuPont denominator
#' @return Named list with resolved configuration
#' @export
resolve_report_config <- function(
    profile = "default",
    price_decomp_metric = NULL,
    price_decomp_numerator = NULL,
    kpi_metric = NULL,
    dupont_numerator = NULL,
    dupont_denominator = NULL
) {
  config <- get_report_profile(profile)

  if (!is.null(price_decomp_metric)) config$price_decomp_metric <- price_decomp_metric
  if (!is.null(price_decomp_numerator)) config$price_decomp_numerator <- price_decomp_numerator
  if (!is.null(kpi_metric)) config$kpi_metric <- kpi_metric
  if (!is.null(dupont_numerator)) config$dupont_numerator <- dupont_numerator
  if (!is.null(dupont_denominator)) config$dupont_denominator <- dupont_denominator

  config
}

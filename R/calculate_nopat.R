#' Calculate NOPAT (Net Operating Profit After Tax)
#'
#' Vectorized function for use inside dplyr::mutate().
#'
#' @param ebit EBIT vector
#' @param dep_amort Depreciation and amortization vector
#' @param depreciation Depreciation vector (to isolate amortization)
#' @param tax_rate Tax rate (default: 0.2375)
#'
#' @return Numeric vector of NOPAT values
#' @export
calculate_nopat <- function(ebit, dep_amort, depreciation, tax_rate = 0.2375) {
  dep_amort <- dplyr::coalesce(dep_amort, 0)
  depreciation <- dplyr::coalesce(depreciation, 0)
  amortization <- pmax(dep_amort - depreciation, 0)

  dplyr::if_else(
    is.na(ebit),
    NA_real_,
    (ebit + amortization) * (1 - tax_rate)
  )
}

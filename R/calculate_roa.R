#' Calculate Return on Assets
#'
#' Vectorized calculation of ROA as a decimal.
#'
#' @param income Numeric vector of income values (e.g., net income, NOPAT)
#' @param assets Numeric vector of total asset values
#' @return Numeric vector of ROA as decimals
#' @export
calculate_roa <- function(income, assets) {
  income / assets
}

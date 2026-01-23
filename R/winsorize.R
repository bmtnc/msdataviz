#' Winsorize a Vector
#'
#' Clips extreme values to specified percentiles.
#'
#' @param x Numeric vector to winsorize
#' @param lower_pct Lower percentile (default: 0.01 for 1st percentile)
#' @param upper_pct Upper percentile (default: 0.99 for 99th percentile)
#' @return Winsorized numeric vector
#' @export
#' @keywords internal
winsorize <- function(x, lower_pct = 0.01, upper_pct = 0.99) {
  if (all(is.na(x))) return(x)

  bounds <- stats::quantile(x, probs = c(lower_pct, upper_pct), na.rm = TRUE)
  pmax(pmin(x, bounds[2]), bounds[1])
}

#' Calculate FCF Conversion
#'
#' @param fcf Free cash flow (numeric vector)
#' @param nopat Net operating profit after tax (numeric vector)
#'
#' @return FCF conversion ratio (numeric vector)
#' @keywords internal
fcf_conversion <- function(fcf, nopat) {
  ifelse(nopat > 0, fcf / nopat, NA_real_)
}

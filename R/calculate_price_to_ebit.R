#' Calculate Price-to-EBIT Ratio
#'
#' @param price Stock price (numeric vector)
#' @param ebit_per_share EBIT per share (numeric vector)
#' @return P/EBIT ratio (numeric vector)
#' @keywords internal
#' @export
calculate_price_to_ebit <- function(price, ebit_per_share) {
  ifelse(ebit_per_share > 0, price / ebit_per_share, NA_real_)
}

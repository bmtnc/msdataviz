#' Calculate Price-to-Free-Cash-Flow Ratio
#'
#' @param price Stock price (numeric vector)
#' @param fcf_per_share Free cash flow per share (numeric vector)
#' @return P/FCF ratio (numeric vector)
#' @keywords internal
#' @export
calculate_price_to_fcf <- function(price, fcf_per_share) {
  ifelse(fcf_per_share > 0, price / fcf_per_share, NA_real_)
}

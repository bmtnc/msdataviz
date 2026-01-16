#' Calculate Price-to-Earnings Ratio
#'
#' @param price Stock price (numeric vector)
#' @param earnings_per_share Earnings per share (numeric vector)
#' @return P/E ratio (numeric vector)
#' @keywords internal
#' @export
calculate_price_to_earnings <- function(price, earnings_per_share) {
  ifelse(earnings_per_share > 0, price / earnings_per_share, NA_real_)
}

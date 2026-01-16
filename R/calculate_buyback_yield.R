#' Calculate Buyback Yield
#'
#' @param buyback_per_share TTM buyback per share (numeric vector)
#' @param price Stock price (numeric vector)
#' @return Buyback yield as decimal (numeric vector)
#' @keywords internal
#' @export
calculate_buyback_yield <- function(buyback_per_share, price) {
  ifelse(price > 0, buyback_per_share / price, NA_real_)
}

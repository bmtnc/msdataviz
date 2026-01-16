#' Calculate Dividend Yield
#'
#' @param dividend_per_share TTM dividend per share (numeric vector)
#' @param price Stock price (numeric vector)
#' @return Dividend yield as decimal (numeric vector)
#' @keywords internal
#' @export
calculate_dividend_yield <- function(dividend_per_share, price) {
  ifelse(price > 0, dividend_per_share / price, NA_real_)
}

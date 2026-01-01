#' Calculate Cumulative Return
#'
#' Calculates cumulative return from a price series.
#'
#' @param prices Numeric vector of prices
#' @return Numeric vector of cumulative returns (0 at start)
#' @export
cumulative_return <- function(prices) {
  prices / prices[1] - 1
}

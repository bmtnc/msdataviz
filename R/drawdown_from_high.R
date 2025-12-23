#' Calculate Drawdown from Running High
#'
#' Computes the percentage drawdown from the cumulative maximum.
#'
#' @param prices Numeric vector of prices
#'
#' @return Numeric vector of drawdowns (negative or zero values)
#' @export
drawdown_from_high <- function(prices) {
  running_max <- cummax(prices)
  (prices - running_max) / running_max
}

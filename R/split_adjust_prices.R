#' Split-Adjust Prices
#'
#' Adjusts close prices for stock splits only (not dividends). Makes all
#' historical prices comparable to the latest price.
#'
#' @param close numeric: Vector of unadjusted close prices
#' @param split_coefficient numeric: Vector of split coefficients (1.0 = no split,
#'   2.0 = 2-for-1 split). Must be same length as close.
#' @return numeric: Vector of split-adjusted close prices
#' @export
#' @keywords internal
split_adjust_prices <- function(close, split_coefficient) {
  if (length(close) != length(split_coefficient)) {
    stop("close and split_coefficient must have the same length")
  }

  if (length(close) == 0) {
    return(numeric(0))
  }

  cumulative_split <- cumprod(split_coefficient)
  final_split <- cumulative_split[length(cumulative_split)]

  close * cumulative_split / final_split
}

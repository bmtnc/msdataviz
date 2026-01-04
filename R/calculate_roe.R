#' Calculate Return on Equity
#'
#' Vectorized calculation of ROE as a decimal.
#'
#' @param income Numeric vector of income values (e.g., net income, NOPAT)
#' @param equity Numeric vector of shareholder equity values
#' @return Numeric vector of ROE as decimals
#' @export
calculate_roe <- function(income, equity) {
  income / equity
}

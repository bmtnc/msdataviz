#' Calculate Shareholder Yield
#'
#' Sum of dividend yield and buyback yield.
#'
#' @param dividend_per_share TTM dividend per share (numeric vector)
#' @param buyback_per_share TTM buyback per share (numeric vector)
#' @param price Stock price (numeric vector)
#' @return Shareholder yield as decimal (numeric vector)
#' @keywords internal
#' @export
calculate_shareholder_yield <- function(dividend_per_share, buyback_per_share, price) {
  ifelse(
    price > 0,
    (dividend_per_share + buyback_per_share) / price,
    NA_real_
  )
}

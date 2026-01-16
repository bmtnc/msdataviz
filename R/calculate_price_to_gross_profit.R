#' Calculate Price-to-Gross-Profit Ratio
#'
#' @param price Stock price (numeric vector)
#' @param gross_profit_per_share Gross profit per share (numeric vector)
#' @return P/Gross Profit ratio (numeric vector)
#' @keywords internal
#' @export
calculate_price_to_gross_profit <- function(price, gross_profit_per_share) {
  ifelse(gross_profit_per_share > 0, price / gross_profit_per_share, NA_real_)
}

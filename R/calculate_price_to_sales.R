#' Calculate Price-to-Sales Ratio
#'
#' @param price Stock price (numeric vector)
#' @param revenue_per_share Revenue per share (numeric vector)
#' @return P/S ratio (numeric vector)
#' @keywords internal
#' @export
calculate_price_to_sales <- function(price, revenue_per_share) {
  ifelse(revenue_per_share > 0, price / revenue_per_share, NA_real_)
}

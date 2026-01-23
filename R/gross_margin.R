#' Calculate Gross Margin
#'
#' @param gross_profit Gross profit (numeric vector)
#' @param revenue Revenue (numeric vector)
#'
#' @return Gross margin as decimal (numeric vector)
#' @export
#' @keywords internal
gross_margin <- function(gross_profit, revenue) {
  ifelse(revenue > 0, gross_profit / revenue, NA_real_)
}

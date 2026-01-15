#' Calculate Gross Return on Invested Capital (GROIC)
#'
#' @param gross_profit Gross profit (numeric vector)
#' @param invested_capital Invested capital (numeric vector)
#'
#' @return GROIC as decimal (numeric vector)
#' @keywords internal
groic <- function(gross_profit, invested_capital) {
  ifelse(invested_capital > 0, gross_profit / invested_capital, NA_real_)
}

#' Calculate Capital Turnover
#'
#' @param revenue Revenue (numeric vector)
#' @param invested_capital Invested capital (numeric vector)
#'
#' @return Capital turnover ratio (numeric vector)
#' @keywords internal
capital_turnover <- function(revenue, invested_capital) {
  ifelse(invested_capital > 0, revenue / invested_capital, NA_real_)
}

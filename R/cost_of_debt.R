#' Calculate Cost of Debt
#'
#' @param interest_expense Interest expense (numeric vector)
#' @param avg_debt Average debt (numeric vector)
#'
#' @return Cost of debt as decimal (numeric vector)
#' @keywords internal
cost_of_debt <- function(interest_expense, avg_debt) {
  ifelse(avg_debt > 0, interest_expense / avg_debt, NA_real_)
}

#' Calculate Interest Coverage Ratio
#'
#' @param ebit Earnings before interest and taxes (numeric vector)
#' @param interest_expense Interest expense (numeric vector)
#'
#' @return Interest coverage ratio (numeric vector)
#' @keywords internal
interest_coverage <- function(ebit, interest_expense) {
  ifelse(interest_expense > 0, ebit / interest_expense, NA_real_)
}

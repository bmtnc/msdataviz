#' Calculate Debt to EBITDA Ratio
#'
#' @param total_debt Total debt (numeric vector)
#' @param ebitda EBITDA (numeric vector)
#'
#' @return Debt to EBITDA ratio (numeric vector)
#' @export
#' @keywords internal
debt_to_ebitda <- function(total_debt, ebitda) {
  ifelse(ebitda > 0, total_debt / ebitda, NA_real_)
}

#' Calculate Free Cash Flow
#'
#' Vectorized function for use inside dplyr::mutate().
#'
#' @param operating_cashflow Operating cash flow vector
#' @param capital_expenditures Capital expenditures vector (positive values)
#'
#' @return Numeric vector of FCF values
#' @export
calculate_fcf <- function(operating_cashflow, capital_expenditures) {
  capex <- dplyr::coalesce(capital_expenditures, 0)
  dplyr::if_else(
    is.na(operating_cashflow),
    NA_real_,
    operating_cashflow - abs(capex)
  )
}

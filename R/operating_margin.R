#' Calculate Operating Margin
#'
#' @param operating_income Operating income (numeric vector)
#' @param revenue Revenue (numeric vector)
#'
#' @return Operating margin as decimal (numeric vector)
#' @export
#' @keywords internal
operating_margin <- function(operating_income, revenue) {
  ifelse(revenue > 0, operating_income / revenue, NA_real_)
}

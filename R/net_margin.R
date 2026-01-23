#' Calculate Net Margin
#'
#' @param net_income Net income (numeric vector)
#' @param revenue Revenue (numeric vector)
#'
#' @return Net margin as decimal (numeric vector)
#' @export
#' @keywords internal
net_margin <- function(net_income, revenue) {
  ifelse(revenue > 0, net_income / revenue, NA_real_)
}

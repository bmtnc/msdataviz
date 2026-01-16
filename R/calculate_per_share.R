#' Calculate Per-Share Value
#'
#' Converts a total value to per-share by dividing by shares outstanding.
#'
#' @param total Total value (numeric vector)
#' @param shares Shares outstanding (numeric vector)
#' @return Per-share value (numeric vector)
#' @keywords internal
#' @export
calculate_per_share <- function(total, shares) {
  ifelse(shares > 0, total / shares, NA_real_)
}

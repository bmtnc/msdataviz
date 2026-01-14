#' Time Series Anomaly Detection via Z-Score
#'
#' Flags values that exceed a z-score threshold.
#'
#' @param x Numeric vector of values
#' @param threshold Number of standard deviations from mean to flag (default: 2)
#' @param direction Which direction to flag: "low", "high", or "both" (default: "both")
#'
#' @return Logical vector of same length as x (TRUE = anomaly)
#' @export
ts_anomaly <- function(x, threshold = 2, direction = c("both", "low", "high")) {
  direction <- match.arg(direction)
  if (length(x) < 2) {
    return(rep(FALSE, length(x)))
  }
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) {
    return(rep(FALSE, length(x)))
  }
  z <- (x - mean(x, na.rm = TRUE)) / s
  switch(direction,
    low = z < -threshold,
    high = z > threshold,
    both = abs(z) > threshold
  )
}

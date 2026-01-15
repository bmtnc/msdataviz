#' Cross-Sectional Anomaly Detection via Z-Score
#'
#' Flags a value as anomalous relative to a cross-sectional distribution using z-score.
#'
#' @param x Numeric value to test
#' @param distribution Numeric vector of cross-sectional values (peers)
#' @param threshold Number of standard deviations from mean to flag (default: 2)
#' @param direction Which direction to flag: "low", "high", or "both" (default: "both")
#'
#' @return Logical scalar (TRUE = anomaly)
#' @export
cross_sectional_anomaly <- function(x, distribution, threshold = 2,
                                    direction = c("both", "low", "high")) {
  direction <- match.arg(direction)
  if (length(distribution) < 2) {
    return(FALSE)
  }
  s <- sd(distribution, na.rm = TRUE)
  if (is.na(s) || s == 0) {
    return(FALSE)
  }
  z <- (x - mean(distribution, na.rm = TRUE)) / s

  switch(direction,
    low = z < -threshold,
    high = z > threshold,
    both = abs(z) > threshold
  )
}

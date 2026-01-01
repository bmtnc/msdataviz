#' Fit Elliptic Envelope for Outlier Detection
#'
#' Fits an elliptic envelope using robust covariance estimation (Minimum
#' Covariance Determinant). Returns Mahalanobis distances and outlier flags.
#' Optionally winsorizes data before fitting.
#'
#' @param x Numeric vector for x-axis values
#' @param y Numeric vector for y-axis values
#' @param contamination Expected proportion of outliers (default: 0.1)
#' @param winsorize_pct Percentile for winsorization (default: 0.02 for 2nd/98th)
#' @return List with: center, cov, distances, threshold, is_outlier, x_winsorized, y_winsorized
#' @export
fit_elliptic_envelope <- function(x, y, contamination = 0.1, winsorize_pct = 0.02) {
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }

  # Remove NA values
  complete_idx <- !is.na(x) & !is.na(y)
  x_clean <- x[complete_idx]
  y_clean <- y[complete_idx]

  if (length(x_clean) < 3) {
    stop("Need at least 3 complete observations")
  }

  # Winsorize to reduce impact of extreme outliers
  x_winsorized <- winsorize(x_clean, lower_pct = winsorize_pct, upper_pct = 1 - winsorize_pct)
  y_winsorized <- winsorize(y_clean, lower_pct = winsorize_pct, upper_pct = 1 - winsorize_pct)

  data_matrix <- cbind(x_winsorized, y_winsorized)

  # Fit robust covariance using MCD
  # quantile.used controls how many points are used (1 - contamination)
  n <- nrow(data_matrix)
  h <- floor((1 - contamination) * n)

  mcd_fit <- MASS::cov.mcd(data_matrix, quantile.used = h)

  center <- mcd_fit$center
  cov_matrix <- mcd_fit$cov

  # Calculate Mahalanobis distances
  distances <- mahalanobis(data_matrix, center, cov_matrix)

  # Chi-squared threshold for 2 dimensions at (1 - contamination) quantile
  threshold <- stats::qchisq(1 - contamination, df = 2)

  # Flag outliers
  is_outlier <- distances > threshold

  # Build result for all original observations (including NAs)
  full_distances <- rep(NA_real_, length(x))
  full_is_outlier <- rep(NA, length(x))
  full_x_winsorized <- rep(NA_real_, length(x))
  full_y_winsorized <- rep(NA_real_, length(x))

  full_distances[complete_idx] <- distances
  full_is_outlier[complete_idx] <- is_outlier
  full_x_winsorized[complete_idx] <- x_winsorized
  full_y_winsorized[complete_idx] <- y_winsorized

  list(
    center = center,
    cov = cov_matrix,
    distances = full_distances,
    threshold = threshold,
    is_outlier = full_is_outlier,
    contamination = contamination,
    x_winsorized = full_x_winsorized,
    y_winsorized = full_y_winsorized,
    winsorize_pct = winsorize_pct
  )
}


#' Generate Ellipse Points for Plotting
#'
#' Generates x,y coordinates for an ellipse boundary from covariance matrix.
#'
#' @param center Numeric vector of length 2 (center x, y)
#' @param cov 2x2 covariance matrix
#' @param level Confidence level (default: 0.9)
#' @param n_points Number of points to generate (default: 100)
#' @return Data frame with x, y columns
#' @keywords internal
generate_ellipse_points <- function(center, cov, level = 0.9, n_points = 100) {
  # Chi-squared quantile for 2 df
  chisq_val <- stats::qchisq(level, df = 2)

  # Eigendecomposition of covariance
  eig <- eigen(cov)
  eigenvalues <- eig$values
  eigenvectors <- eig$vectors

  # Generate points on unit circle
  angles <- seq(0, 2 * pi, length.out = n_points)
  unit_circle <- cbind(cos(angles), sin(angles))

  # Scale by sqrt(eigenvalues * chi-sq) and rotate
  scaled <- unit_circle %*% diag(sqrt(eigenvalues * chisq_val))
  rotated <- scaled %*% t(eigenvectors)

  # Translate to center
  ellipse_points <- sweep(rotated, 2, center, "+")

  data.frame(x = ellipse_points[, 1], y = ellipse_points[, 2])
}

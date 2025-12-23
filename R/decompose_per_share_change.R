#' Decompose Per-Share Change into Growth and Share Count Effects
#'
#' Separates per-share fundamental changes into total growth effect vs share count effect.
#'
#' @param total_fundamental Numeric vector of total fundamental values
#' @param base_total_fundamental Scalar base total fundamental value
#' @param shares_outstanding Numeric vector of shares outstanding
#' @param fundamental_per_share Numeric vector of fundamental per share
#' @param base_fundamental_per_share Scalar base fundamental per share
#'
#' @return List with growth_effect and share_count_effect vectors
#' @export
decompose_per_share_change <- function(
    total_fundamental,
    base_total_fundamental,
    shares_outstanding,
    fundamental_per_share,
    base_fundamental_per_share
) {
  actual_per_share_change <- fundamental_per_share - base_fundamental_per_share

  growth_per_share_effect <- (total_fundamental - base_total_fundamental) / shares_outstanding
  share_count_per_share_effect <- actual_per_share_change - growth_per_share_effect

  list(
    growth_effect = growth_per_share_effect,
    share_count_effect = share_count_per_share_effect,
    actual_change = actual_per_share_change
  )
}

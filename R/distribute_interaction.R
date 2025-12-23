#' Distribute Interaction Term Proportionally
#'
#' Distributes an interaction term between two contributions proportionally to their magnitudes.
#'
#' @param contrib_a Numeric vector of first contribution values
#' @param contrib_b Numeric vector of second contribution values
#' @param interaction Numeric vector of interaction term values
#'
#' @return List with a_adjusted and b_adjusted vectors
#' @export
distribute_interaction <- function(contrib_a, contrib_b, interaction) {
  total <- abs(contrib_a) + abs(contrib_b)

  a_share <- ifelse(total > 0, abs(contrib_a) / total, 0.5)
  b_share <- ifelse(total > 0, abs(contrib_b) / total, 0.5)

  list(
    a_adjusted = contrib_a + interaction * a_share,
    b_adjusted = contrib_b + interaction * b_share
  )
}

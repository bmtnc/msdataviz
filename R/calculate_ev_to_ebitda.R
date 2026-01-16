#' Calculate EV-to-EBITDA Ratio
#'
#' @param ev_per_share Enterprise value per share (numeric vector)
#' @param ebitda_per_share EBITDA per share (numeric vector)
#' @return EV/EBITDA ratio (numeric vector)
#' @keywords internal
#' @export
calculate_ev_to_ebitda <- function(ev_per_share, ebitda_per_share) {
  ifelse(ebitda_per_share > 0, ev_per_share / ebitda_per_share, NA_real_)
}

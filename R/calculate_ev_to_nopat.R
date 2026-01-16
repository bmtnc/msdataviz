#' Calculate EV-to-NOPAT Ratio
#'
#' @param ev_per_share Enterprise value per share (numeric vector)
#' @param nopat_per_share NOPAT per share (numeric vector)
#' @return EV/NOPAT ratio (numeric vector)
#' @keywords internal
#' @export
calculate_ev_to_nopat <- function(ev_per_share, nopat_per_share) {
  ifelse(nopat_per_share > 0, ev_per_share / nopat_per_share, NA_real_)
}

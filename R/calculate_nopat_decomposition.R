#' Calculate NOPAT Decomposition
#'
#' Decomposes cumulative change in NOPAT into ROIC effect (on base capital)
#' and capital deployment effect (from incremental capital).
#'
#' @param data Data frame with columns: date, ic (invested capital), roic
#' @param base_date Date to use as baseline (default: min date in data)
#'
#' @return Data frame with columns: date, nopat, nopat_change, roic_effect,
#'   capital_effect
#' @export
calculate_nopat_decomposition <- function(data, base_date = NULL) {
  required_cols <- c("date", "ic", "roic")
  avpipeline::validate_df_cols(data, required_cols)
  avpipeline::validate_non_empty(data, "data")

  data <- data %>%
    dplyr::arrange(date)

  if (is.null(base_date)) {
    base_date <- min(data$date)
  }

  base_idx <- which(data$date == base_date)
  if (length(base_idx) == 0) {
    stop("base_date not found in data: ", base_date)
  }

  base_ic <- data$ic[base_idx]
  base_roic <- data$roic[base_idx]
  base_nopat <- base_ic * base_roic

  data %>%
    dplyr::filter(date >= base_date) %>%
    dplyr::mutate(
      nopat = ic * roic,
      nopat_change = nopat - base_nopat,
      roic_effect = base_ic * (roic - base_roic),
      capital_effect = (ic - base_ic) * roic
    ) %>%
    dplyr::select(date, nopat, nopat_change, roic_effect, capital_effect)
}

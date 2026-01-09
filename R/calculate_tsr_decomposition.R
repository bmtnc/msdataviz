#' Calculate TSR Decomposition
#'
#' Decomposes Total Shareholder Return (adjusted close) into market cap growth
#' and share count effect. Market cap is calculated as adjusted_close × shares,
#' so the gap captures purely the impact of share count changes on per-share returns.
#'
#' @param data Data frame with columns: date, adjusted_close, shares
#' @param base_date Date to use as base for decomposition (first row if NULL)
#'
#' @return Data frame with decomposition columns:
#'   - date, adjusted_close, shares, market_cap
#'   - tsr: cumulative % change in adjusted close (total shareholder return)
#'   - market_cap_growth: cumulative % change in market cap
#'   - share_count_effect: tsr - market_cap_growth (the residual)
#' @export
calculate_tsr_decomposition <- function(data, base_date = NULL) {
  required_cols <- c("date", "adjusted_close", "shares")
  avpipeline::validate_df_cols(data, required_cols)
  avpipeline::validate_non_empty(data, "data")

  data <- data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(market_cap = adjusted_close * shares)

  if (is.null(base_date)) {
    base_date <- min(data$date)
  }

  base_row <- data %>%
    dplyr::filter(date == base_date) %>%
    dplyr::slice(1)

  if (nrow(base_row) == 0) {
    base_date <- data$date[which.min(abs(data$date - base_date))]
    base_row <- data %>%
      dplyr::filter(date == base_date) %>%
      dplyr::slice(1)
  }

  base_adjusted_close <- base_row$adjusted_close
  base_market_cap <- base_row$market_cap

  data %>%
    dplyr::filter(date >= base_date) %>%
    dplyr::mutate(
      tsr = adjusted_close / base_adjusted_close - 1,
      market_cap_growth = market_cap / base_market_cap - 1,
      share_count_effect = tsr - market_cap_growth
    )
}

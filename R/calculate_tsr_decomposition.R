#' Calculate TSR Decomposition
#'
#' Decomposes Total Shareholder Return into three additive components:
#' market cap growth, dividend effect, and share count effect.
#'
#' @param data Data frame with columns: date, adjusted_close, close,
#'   split_coefficient, shares
#' @param base_date Date to use as base for decomposition (first row if NULL)
#'
#' @return Data frame with decomposition columns:
#'   - tsr: cumulative % change in adjusted close (total shareholder return)
#'   - market_cap_growth: cumulative % change in market cap
#'   - dividend_effect: contribution from dividend reinvestment
#'   - share_count_effect: contribution from buybacks/dilution
#' @export
calculate_tsr_decomposition <- function(data, base_date = NULL) {
  required_cols <- c("date", "adjusted_close", "close", "split_coefficient", "shares")
  avpipeline::validate_df_cols(data, required_cols)
  avpipeline::validate_non_empty(data, "data")

  data <- data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      split_adjusted_close = split_adjust_prices(close, split_coefficient),
      market_cap = split_adjusted_close * shares
    )

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
  base_split_adjusted_close <- base_row$split_adjusted_close
  base_market_cap <- base_row$market_cap

  data %>%
    dplyr::filter(date >= base_date) %>%
    dplyr::mutate(
      tsr = adjusted_close / base_adjusted_close - 1,
      split_adjusted_return = split_adjusted_close / base_split_adjusted_close - 1,
      market_cap_growth = market_cap / base_market_cap - 1,
      dividend_effect = tsr - split_adjusted_return,
      share_count_effect = split_adjusted_return - market_cap_growth
    )
}

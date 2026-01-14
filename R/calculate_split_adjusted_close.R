#' Calculate Split-Adjusted Close Prices
#'
#' Adds a split_adjusted_close column to price data. Adjusts for stock splits
#' only (not dividends), making historical prices comparable to the latest price.
#'
#' @param data data.frame: Price data with columns: ticker, date, close, split_coefficient
#' @return data.frame: Input data with added split_adjusted_close column
#' @export
calculate_split_adjusted_close <- function(data) {
  avpipeline::validate_df_cols(data, c("ticker", "date", "close", "split_coefficient"))
  avpipeline::validate_non_empty(data, "data")

  data %>%
    dplyr::arrange(ticker, date) %>%
    dplyr::group_by(ticker) %>%
    dplyr::mutate(
      split_adjusted_close = split_adjust_prices(close, split_coefficient)
    ) %>%
    dplyr::ungroup()
}

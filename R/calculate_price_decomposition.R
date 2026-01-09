#' Calculate Price Decomposition
#'
#' Decomposes cumulative price returns into fundamental growth and valuation effect.
#' Uses the residual method: valuation_effect = price_return - fundamental_growth.
#'
#' @param data Data frame with columns: date, price, fundamental_per_share
#' @param base_date Date to use as base for decomposition (first row if NULL)
#'
#' @return Data frame with decomposition columns:
#'   - date, price, fundamental_per_share, multiple
#'   - fundamental_growth: cumulative % change in fundamental per share
#'   - price_return: cumulative % change in price
#'   - valuation_effect: price_return - fundamental_growth (the residual)
#' @export
calculate_price_decomposition <- function(data, base_date = NULL) {
  required_cols <- c("date", "price", "fundamental_per_share")
  avpipeline::validate_df_cols(data, required_cols)
  avpipeline::validate_non_empty(data, "data")

  data <- data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      multiple = price / fundamental_per_share
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

  base_price <- base_row$price
  base_fundamental_per_share <- base_row$fundamental_per_share

  data %>%
    dplyr::filter(date >= base_date) %>%
    dplyr::mutate(
      fundamental_growth = fundamental_per_share / base_fundamental_per_share - 1,
      price_return = price / base_price - 1,
      valuation_effect = price_return - fundamental_growth
    )
}

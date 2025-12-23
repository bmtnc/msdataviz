#' Calculate Price Decomposition
#'
#' Decomposes cumulative price changes into fundamental growth, share count, and valuation contributions.
#'
#' @param data Data frame with columns: date, price, fundamental_per_share, shares_outstanding
#' @param base_date Date to use as base for decomposition (first row if NULL)
#'
#' @return Data frame with decomposition columns added
#' @export
calculate_price_decomposition <- function(data, base_date = NULL) {
  required_cols <- c("date", "price", "fundamental_per_share", "shares_outstanding")
  avpipeline::validate_df_cols(data, required_cols)
  avpipeline::validate_non_empty(data, "data")

  data <- data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      total_fundamental = fundamental_per_share * shares_outstanding,
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
  base_total_fundamental <- base_row$total_fundamental
  base_multiple <- base_row$multiple

  result <- data %>%
    dplyr::filter(date >= base_date) %>%
    dplyr::mutate(
      price_change = price - base_price,
      fundamental_per_share_change = fundamental_per_share - base_fundamental_per_share,
      multiple_change = multiple - base_multiple,
      fundamental_contribution_raw = fundamental_per_share_change * base_multiple,
      multiple_contribution_raw = base_fundamental_per_share * multiple_change,
      interaction_term = fundamental_per_share_change * multiple_change
    )

  adjusted <- distribute_interaction(
    result$fundamental_contribution_raw,
    result$multiple_contribution_raw,
    result$interaction_term
  )

  per_share <- decompose_per_share_change(
    result$total_fundamental,
    base_total_fundamental,
    result$shares_outstanding,
    result$fundamental_per_share,
    base_fundamental_per_share
  )

  result %>%
    dplyr::mutate(
      fundamental_contribution = adjusted$a_adjusted,
      multiple_contribution = adjusted$b_adjusted,
      nopat_growth_contribution = ifelse(
        abs(per_share$actual_change) > 1e-10,
        fundamental_contribution * per_share$growth_effect / per_share$actual_change,
        fundamental_contribution * 0.5
      ),
      share_count_contribution = ifelse(
        abs(per_share$actual_change) > 1e-10,
        fundamental_contribution * per_share$share_count_effect / per_share$actual_change,
        fundamental_contribution * 0.5
      )
    ) %>%
    dplyr::select(
      date, price, fundamental_per_share, shares_outstanding, total_fundamental, multiple,
      price_change, fundamental_contribution, multiple_contribution,
      nopat_growth_contribution, share_count_contribution
    )
}

#' Calculate Share Count Decomposition
#'
#' Decomposes cumulative per-share metric growth into total metric growth (numerator)
#' and share count effect (denominator). Uses the residual method.
#'
#' Note: This function does not filter negative metric values. Percentage changes
#' may not be meaningful when the metric crosses zero or is negative.
#'
#' @param data Data frame with columns: date, metric, shares
#' @param base_date Date to use as base for decomposition (first row if NULL)
#'
#' @return Data frame with decomposition columns:
#'   - date, metric, shares, metric_per_share
#'   - organic_growth: cumulative % change in total metric (numerator effect)
#'   - per_share_growth: cumulative % change in metric per share
#'   - share_effect: per_share_growth - organic_growth (denominator effect)
#' @export
calculate_share_count_decomposition <- function(data, base_date = NULL) {
  required_cols <- c("date", "metric", "shares")
  avpipeline::validate_df_cols(data, required_cols)
  avpipeline::validate_non_empty(data, "data")

  data <- data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(metric_per_share = metric / shares)

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

  base_metric <- base_row$metric
  base_metric_per_share <- base_row$metric_per_share

  data %>%
    dplyr::filter(date >= base_date) %>%
    dplyr::mutate(
      organic_growth = metric / base_metric - 1,
      per_share_growth = metric_per_share / base_metric_per_share - 1,
      share_effect = per_share_growth - organic_growth
    )
}

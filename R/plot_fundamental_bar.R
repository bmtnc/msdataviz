#' Create Fundamental KPI Bar Plot
#'
#' Creates a quarterly bar chart for a fundamental metric.
#'
#' @param data Data frame with columns: fiscalDateEnding, value
#' @param ticker Character string for the ticker symbol (used in title)
#' @param metric_name Character string for the metric name (used in title/labels)
#'
#' @return A ggplot2 object
#' @export
plot_fundamental_bar <- function(data, ticker, metric_name) {
  avpipeline::validate_df_cols(data, c("fiscalDateEnding", "value"))
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")
  avpipeline::validate_character_scalar(metric_name, allow_empty = FALSE, name = "metric_name")

  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = fiscalDateEnding, y = value)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
    ggplot2::labs(
      title = paste0(ticker, ": ", metric_name),
      x = "Fiscal Date Ending",
      y = metric_name
    ) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )

  p <- add_scale_suffix(p, max(data$value, na.rm = TRUE))

  p
}

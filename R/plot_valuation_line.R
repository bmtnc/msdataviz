#' Create Valuation Line Plot with Callout
#'
#' Creates a daily line plot for a valuation metric with a callout on the most recent value.
#'
#' @param data Data frame with columns: date, value
#' @param ticker Character string for the ticker symbol (used in title)
#' @param metric_name Character string for the metric name (used in title/labels)
#'
#' @return A ggplot2 object
#' @export
plot_valuation_line <- function(data, ticker, metric_name) {
  avpipeline::validate_df_cols(data, c("date", "value"))
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")
  avpipeline::validate_character_scalar(metric_name, allow_empty = FALSE, name = "metric_name")

  most_recent <- data %>%
    dplyr::slice_tail(n = 1)

  date_range <- diff(range(data$date))
  max_value <- max(data$value, na.rm = TRUE)

  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line(color = "darkgreen", alpha = 0.8, linewidth = 1) +
    ggplot2::geom_point(
      data = most_recent,
      ggplot2::aes(x = date, y = value),
      color = "black",
      size = 3
    ) +
    ggplot2::geom_label(
      data = most_recent,
      ggplot2::aes(
        x = date,
        y = value,
        label = scales::comma(value, accuracy = 0.01)
      ),
      nudge_x = as.numeric(date_range) * 0.04,
      nudge_y = max_value * 0.02,
      color = "black",
      fill = "white",
      alpha = 0.8,
      size = 2.5,
      fontface = "bold"
    ) +
    ggplot2::labs(
      title = paste0(ticker, ": ", metric_name),
      x = "Date",
      y = metric_name
    ) +
    ggplot2::coord_cartesian(
      xlim = c(
        min(data$date),
        max(data$date) + as.numeric(date_range) * 0.12
      )
    ) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )

  p <- add_scale_suffix(p, max_value)

  p
}

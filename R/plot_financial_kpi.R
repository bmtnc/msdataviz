#' Plot Financial KPI Over Time
#'
#' Creates a bar chart showing a financial KPI over time.
#'
#' @param data Data frame with date and value columns
#' @param ticker Character string for the ticker symbol
#' @param date_col Name of the date column (default: "date")
#' @param value_col Name of the value column (default: "value")
#' @param metric_name Display name for the metric (used in title and y-axis)
#' @param fill_color Bar fill color (default: "steelblue")
#' @param title Optional custom title (default: auto-generated from ticker and metric_name)
#'
#' @return A ggplot2 object
#' @export
plot_financial_kpi <- function(
    data,
    ticker,
    date_col = "date",
    value_col = "value",
    metric_name,
    fill_color = "steelblue",
    title = NULL
) {
  avpipeline::validate_df_cols(data, c(date_col, value_col))
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")
  avpipeline::validate_character_scalar(metric_name, allow_empty = FALSE, name = "metric_name")

  plot_data <- data %>%
    dplyr::rename(date = !!date_col, value = !!value_col) %>%
    dplyr::arrange(date)

  plot_title <- if (is.null(title)) {
    paste0(ticker, ": ", metric_name)
  } else {
    title
  }

  date_range <- range(plot_data$date)
  date_buffer <- as.numeric(diff(date_range)) * 0.05

  dates_sorted <- sort(unique(plot_data$date))
  bar_width <- if (length(dates_sorted) > 1) {
    median(diff(dates_sorted))
  } else {
    90
  }

  plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_col(
      fill = fill_color,
      width = bar_width,
      color = "gray80",
      linewidth = 0.2
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(date_range[1] - bar_width / 2, date_range[2] + date_buffer)
    ) +
    ggplot2::labs(
      title = plot_title,
      x = NULL,
      y = metric_name
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0)
    )
}

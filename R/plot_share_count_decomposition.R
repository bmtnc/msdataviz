#' Plot Share Count Decomposition
#'
#' Creates a stacked bar chart showing organic growth and share count effect.
#' The bars sum to per-share growth: organic + share_effect = per_share_growth.
#'
#' @param data Data frame from calculate_share_count_decomposition with columns:
#'   date, organic_growth, share_effect, per_share_growth
#' @param ticker Character string for the ticker symbol
#' @param metric_name Display name for the metric (e.g., "NOPAT", "Earnings")
#' @param base_date Date object for the start of the decomposition period
#'
#' @return A ggplot2 object
#' @export
plot_share_count_decomposition <- function(
    data,
    ticker,
    metric_name = "NOPAT",
    base_date = NULL
) {
  required_cols <- c("date", "organic_growth", "share_effect", "per_share_growth")
  avpipeline::validate_df_cols(data, required_cols)
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  if (is.null(base_date)) {
    base_date <- min(data$date)
  }

  organic_label <- paste0("\u0394 ", metric_name, " (Organic)")
  share_label <- "Share Count Effect"

  plot_data <- data %>%
    dplyr::select(date, organic_growth, share_effect) %>%
    tidyr::pivot_longer(
      cols = c(organic_growth, share_effect),
      names_to = "component",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      component = factor(
        component,
        levels = c("organic_growth", "share_effect"),
        labels = c(organic_label, share_label)
      )
    )

  color_values <- setNames(
    c("#7A9DC7", "gray60"),
    c(organic_label, share_label)
  )

  last_row <- data %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::slice(1)

  subtitle_text <- paste0(
    "Cumulative Per-Share Growth: ", scales::percent(last_row$per_share_growth, accuracy = 0.1), "\n",
    "Organic (", metric_name, "): ", scales::percent(last_row$organic_growth, accuracy = 0.1), "\n",
    "Share Count Effect: ", scales::percent(last_row$share_effect, accuracy = 0.1)
  )

  date_range <- range(data$date)
  date_buffer <- as.numeric(diff(date_range)) * 0.05

  dates_sorted <- sort(unique(data$date))
  bar_width <- if (length(dates_sorted) > 1) {
    median(diff(dates_sorted))
  } else {
    90
  }

  data %>%
    ggplot2::ggplot(ggplot2::aes(x = date)) +
    ggplot2::geom_col(
      data = plot_data,
      ggplot2::aes(x = date, y = value, fill = component),
      width = bar_width,
      color = "gray80",
      linewidth = 0.2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = per_share_growth),
      color = "black",
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = last_row,
      ggplot2::aes(y = per_share_growth),
      color = "black",
      size = 3
    ) +
    ggplot2::geom_text(
      data = last_row,
      ggplot2::aes(
        y = per_share_growth,
        label = scales::percent(per_share_growth, accuracy = 0.1)
      ),
      color = "black",
      hjust = -0.3,
      size = 3.5
    ) +
    ggplot2::scale_fill_manual(values = color_values) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(date_range[1] - bar_width / 2, date_range[2] + date_buffer)
    ) +
    ggplot2::labs(
      title = paste0(ticker, ": Per-Share Growth Decomposition"),
      subtitle = subtitle_text,
      x = NULL,
      y = paste0("Cumulative ", metric_name, " Per Share Growth"),
      fill = "",
      caption = paste0(
        "Methodology:\n",
        "Organic Growth = \u0394 ", metric_name, " (total, not per share)\n",
        "Share Count Effect = Per-Share Growth - Organic Growth\n",
        "Positive effect = buybacks; Negative effect = dilution\n",
        "Start Date: ", base_date
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0),
      plot.subtitle = ggplot2::element_text(hjust = 0),
      plot.caption = ggplot2::element_text(hjust = 0, size = 8, color = "gray50")
    )
}

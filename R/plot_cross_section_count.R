#' Plot Cross-Section Count Over Time
#'
#' Creates a bar chart showing the count of observations in a cross-section over time.
#' Designed to be stacked below a main chart to show sample size stability.
#'
#' @param data Data frame with date and count columns
#' @param date_col Name of the date column (default: "date")
#' @param count_col Name of the count column (default: "n")
#' @param title Title for the chart (default: "Sample Size")
#' @param y_label Y-axis label for alignment with main chart (default: NULL)
#' @param xlim Explicit x-axis limits as c(min_date, max_date). If NULL, calculated from data.
#' @param highlight_threshold Fraction of max count below which bars are highlighted (default: 0.5)
#' @param bar_color Color for normal bars (default: "#546E7A")
#' @param highlight_color Color for highlighted bars (default: "#E57373")
#'
#' @return A ggplot2 object
#' @export
plot_cross_section_count <- function(
    data,
    date_col = "date",
    count_col = "n",
    title = "Sample Size",
    y_label = NULL,
    xlim = NULL,
    highlight_threshold = 0.5,
    bar_color = "#546E7A",
    highlight_color = "#E57373"
) {
  avpipeline::validate_df_cols(data, c(date_col, count_col))

  if (nrow(data) == 0 || all(is.na(data[[count_col]]))) {
    return(NULL)
  }

  plot_data <- data %>%
    dplyr::filter(!is.na(.data[[count_col]])) %>%
    dplyr::arrange(.data[[date_col]])

  if (nrow(plot_data) == 0) {
    return(NULL)
  }

  # Calculate date range with 8% buffer to match other plots
  date_range <- range(plot_data[[date_col]])
  date_buffer <- as.numeric(diff(date_range)) * 0.08

  # Use explicit xlim if provided, otherwise calculate
  if (is.null(xlim)) {
    xlim <- c(date_range[1], date_range[2] + date_buffer)
  }

  min_count <- min(plot_data[[count_col]], na.rm = TRUE)
  max_count <- max(plot_data[[count_col]], na.rm = TRUE)
  threshold_value <- max_count * highlight_threshold

  # Calculate dynamic y-axis range with padding
  y_range <- max_count - min_count
  y_padding <- max(y_range * 0.15, 2)
  y_min <- max(0, min_count - y_padding)
  y_max <- max_count + y_padding

  # Calculate bar width based on date spacing
  dates_sorted <- sort(unique(plot_data[[date_col]]))
  bar_width <- if (length(dates_sorted) > 1) {
    median(diff(dates_sorted)) * 0.8
  } else {
    60
  }

  plot_data <- plot_data %>%
    dplyr::mutate(
      is_low = .data[[count_col]] < threshold_value,
      fill_color = ifelse(is_low, highlight_color, bar_color),
      xmin = .data[[date_col]] - bar_width / 2,
      xmax = .data[[date_col]] + bar_width / 2,
      ymin = y_min,
      ymax = .data[[count_col]]
    )

  ggplot2::ggplot(plot_data) +
    ggplot2::geom_rect(
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill_color),
      color = "white",
      linewidth = 0.2
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(
      limits = c(y_min, y_max),
      breaks = scales::pretty_breaks(n = 3)
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = xlim
    ) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = y_label
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9, color = "gray50", hjust = 0),
      plot.margin = ggplot2::margin(t = 5, r = 20, b = 20, l = 20)
    )
}

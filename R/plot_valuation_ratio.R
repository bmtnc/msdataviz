#' Create Valuation Ratio Chart
#'
#' Creates a line chart showing valuation ratio with reference group median.
#'
#' @param data Data frame with columns: date, valuation_ratio, sector_valuation_ratio
#' @param ticker Character string for the ticker symbol
#' @param metric_name Metric name for y-axis label
#' @param reference_name Reference group name for legend
#' @param n_reference_stocks Number of stocks in reference group for subtitle
#'
#' @return A ggplot2 object
#' @export
plot_valuation_ratio <- function(
    data,
    ticker,
    metric_name = "EV to NOPAT (per share)",
    reference_name = "Sector",
    n_reference_stocks = NULL
) {
  avpipeline::validate_df_cols(data, c("date", "valuation_ratio"))
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  has_reference <- "sector_valuation_ratio" %in% names(data)

  # Build subtitle with population count
  subtitle <- if (!is.null(n_reference_stocks)) {
    paste0("n = ", n_reference_stocks, " stocks in ", reference_name)
  } else {
    NULL
  }

  # Legend label for reference line
  reference_legend <- paste0(reference_name, " (Median)")

  if (has_reference) {
    last_row <- data %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::slice(1)

    # Calculate x-axis range with 5% buffer on right
    date_range <- range(data$date)
    date_buffer <- as.numeric(diff(date_range)) * 0.05

    data %>%
      ggplot2::ggplot(ggplot2::aes(x = date)) +
      # Reference line: very thin, gray
      ggplot2::geom_line(
        ggplot2::aes(y = sector_valuation_ratio, color = reference_legend),
        linewidth = 0.3
      ) +
      # Ticker line: thicker, blue
      ggplot2::geom_line(
        ggplot2::aes(y = valuation_ratio, color = ticker),
        linewidth = 0.8
      ) +
      # Callouts for both lines
      ggplot2::geom_point(
        data = last_row,
        ggplot2::aes(y = valuation_ratio),
        color = "steelblue",
        size = 3
      ) +
      ggplot2::geom_text(
        data = last_row,
        ggplot2::aes(y = valuation_ratio, label = sprintf("%.1fx", valuation_ratio)),
        color = "steelblue",
        hjust = -0.2,
        size = 3.5
      ) +
      ggplot2::geom_point(
        data = last_row,
        ggplot2::aes(y = sector_valuation_ratio),
        color = "gray50",
        size = 2.5
      ) +
      ggplot2::geom_text(
        data = last_row,
        ggplot2::aes(y = sector_valuation_ratio, label = sprintf("%.1fx", sector_valuation_ratio)),
        color = "gray50",
        hjust = -0.2,
        size = 3
      ) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(x, "x")) +
      ggplot2::scale_x_date(
        date_breaks = "1 year",
        date_labels = "%Y",
        limits = c(date_range[1], date_range[2] + date_buffer)
      ) +
      ggplot2::scale_color_manual(
        values = stats::setNames(c("steelblue", "gray50"), c(ticker, reference_legend))
      ) +
      ggplot2::labs(
        title = NULL,
        subtitle = subtitle,
        x = NULL,
        y = metric_name,
        color = NULL
      )
  } else {
    data %>%
      ggplot2::ggplot(ggplot2::aes(x = date, y = valuation_ratio)) +
      ggplot2::geom_line(color = "steelblue", linewidth = 0.8) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(x, "x")) +
      ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      ggplot2::labs(
        title = NULL,
        x = NULL,
        y = metric_name
      )
  }
}

#' Create Valuation Ratio Chart
#'
#' Creates a line chart showing valuation ratio with sector reference.
#'
#' @param data Data frame with columns: date, valuation_ratio, sector_valuation_ratio
#' @param ticker Character string for the ticker symbol
#' @param metric_name Metric name for labeling (e.g., "P/Net Income")
#' @param sector_name Sector name for legend
#'
#' @return A ggplot2 object
#' @export
plot_valuation_ratio <- function(data, ticker, metric_name = "P/E", sector_name = "Sector") {
  avpipeline::validate_df_cols(data, c("date", "valuation_ratio"))
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  has_sector <- "sector_valuation_ratio" %in% names(data)

  if (has_sector) {
    # Get last points for callouts
    last_row <- data %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::slice(1)

    data %>%
      ggplot2::ggplot(ggplot2::aes(x = date)) +
      # Sector line: thin, solid, gray
      ggplot2::geom_line(
        ggplot2::aes(y = sector_valuation_ratio, color = sector_name),
        linewidth = 0.5
      ) +
      # Ticker line: thicker, solid, blue
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
      ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      ggplot2::scale_color_manual(
        values = stats::setNames(c("steelblue", "gray50"), c(ticker, sector_name))
      ) +
      ggplot2::labs(
        title = paste0(ticker, " ", metric_name, " Ratio vs ", sector_name),
        x = NULL,
        y = metric_name,
        color = NULL
      )
  } else {
    # Single line plot
    data %>%
      ggplot2::ggplot(ggplot2::aes(x = date, y = valuation_ratio)) +
      ggplot2::geom_line(color = "steelblue", linewidth = 0.8) +
      ggplot2::scale_y_continuous(labels = function(x) paste0(x, "x")) +
      ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      ggplot2::labs(
        title = paste0(ticker, " ", metric_name, " Ratio"),
        x = NULL,
        y = metric_name
      )
  }
}

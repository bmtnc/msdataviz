#' Create Cumulative Return Chart
#'
#' Creates a line chart showing cumulative returns with optional sector overlay.
#' Includes data callouts for both lines when sector data is present.
#'
#' @param data Data frame with columns: date, cumulative_return, and optionally sector_cumulative_return
#' @param ticker Character string for the ticker symbol
#' @param sector_name Optional sector name for legend (default: "Sector")
#'
#' @return A ggplot2 object
#' @export
plot_cumulative_return <- function(data, ticker, sector_name = "Sector") {
  avpipeline::validate_df_cols(data, c("date", "cumulative_return"))
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  has_sector <- "sector_cumulative_return" %in% names(data)

  if (has_sector) {
    # Get last points for callouts
    last_row <- data %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::slice(1)

    # Build plot with separate styled lines
    data %>%
      ggplot2::ggplot(ggplot2::aes(x = date)) +
      # Sector line: thin, solid, gray
      ggplot2::geom_line(
        ggplot2::aes(y = sector_cumulative_return, color = sector_name),
        linewidth = 0.5
      ) +
      # Ticker line: thicker, solid, blue
      ggplot2::geom_line(
        ggplot2::aes(y = cumulative_return, color = ticker),
        linewidth = 0.8
      ) +
      # Callouts for both lines
      ggplot2::geom_point(
        data = last_row,
        ggplot2::aes(y = cumulative_return),
        color = "steelblue",
        size = 3
      ) +
      ggplot2::geom_text(
        data = last_row,
        ggplot2::aes(y = cumulative_return, label = scales::percent(cumulative_return, accuracy = 0.1)),
        color = "steelblue",
        hjust = -0.2,
        size = 3.5
      ) +
      ggplot2::geom_point(
        data = last_row,
        ggplot2::aes(y = sector_cumulative_return),
        color = "gray50",
        size = 2.5
      ) +
      ggplot2::geom_text(
        data = last_row,
        ggplot2::aes(y = sector_cumulative_return, label = scales::percent(sector_cumulative_return, accuracy = 0.1)),
        color = "gray50",
        hjust = -0.2,
        size = 3
      ) +
      ggplot2::scale_y_continuous(labels = scales::percent_format()) +
      ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      ggplot2::scale_color_manual(
        values = stats::setNames(c("steelblue", "gray50"), c(ticker, sector_name))
      ) +
      ggplot2::labs(
        title = paste0(ticker, " Cumulative Return vs ", sector_name),
        x = NULL,
        y = "Cumulative Return",
        color = NULL
      )
  } else {
    # Single line plot (no sector data)
    data %>%
      ggplot2::ggplot(ggplot2::aes(x = date, y = cumulative_return)) +
      ggplot2::geom_line(color = "steelblue", linewidth = 0.8) +
      ggplot2::scale_y_continuous(labels = scales::percent_format()) +
      ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      ggplot2::labs(
        title = paste0(ticker, " Cumulative Return"),
        x = NULL,
        y = "Cumulative Return"
      )
  }
}

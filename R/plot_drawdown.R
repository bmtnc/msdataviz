#' Create Drawdown from Highs Chart
#'
#' Creates an area chart showing rolling drawdown from cumulative highs.
#'
#' @param data Data frame with columns: date, and either price or drawdown
#' @param ticker Character string for the ticker symbol
#'
#' @return A ggplot2 object
#' @export
plot_drawdown <- function(data, ticker) {
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  # Calculate drawdown if not provided
  if ("drawdown" %in% names(data)) {
    plot_data <- data
  } else {
    avpipeline::validate_df_cols(data, c("date", "price"))
    plot_data <- data %>%
      dplyr::mutate(drawdown = drawdown_from_high(price))
  }

  plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = drawdown)) +
    ggplot2::geom_area(fill = "#CC5500", alpha = 0.7) +
    ggplot2::geom_line(color = "#CC5500", linewidth = 0.5) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::labs(
      title = paste0(ticker, " Drawdown from Highs"),
      x = NULL,
      y = "Drawdown"
    )
}

#' Create Price Line Chart
#'
#' Creates a simple price line chart for a stock.
#'
#' @param data Data frame with columns: date, price
#' @param ticker Character string for the ticker symbol
#'
#' @return A ggplot2 object
#' @export
plot_price <- function(data, ticker) {
  avpipeline::validate_df_cols(data, c("date", "price"))
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  data %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = price)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 0.8) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::labs(
      title = paste0(ticker, " Stock Price"),
      x = NULL,
      y = "Price"
    )
}

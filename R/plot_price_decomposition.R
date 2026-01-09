#' Plot Price Return Decomposition
#'
#' Creates a stacked area chart showing fundamental growth and valuation effect.
#' The areas sum to price return: fundamental_growth + valuation_effect = price_return.
#'
#' @param data Data frame from calculate_price_decomposition with columns:
#'   date, fundamental_growth, valuation_effect, price_return
#' @param ticker Character string for the ticker symbol
#' @param metric_name Display name for the metric (e.g., "NOPAT", "Earnings")
#' @param base_date Date object for the start of the decomposition period
#' @param numerator Valuation numerator label: "price" or "ev" (default: "price")
#'
#' @return A ggplot2 object
#' @export
plot_price_decomposition <- function(
    data,
    ticker,
    metric_name = "NOPAT",
    base_date = NULL,
    numerator = "price"
) {
  required_cols <- c("date", "fundamental_growth", "valuation_effect", "price_return")
  avpipeline::validate_df_cols(data, required_cols)
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  if (is.null(base_date)) {
    base_date <- min(data$date)
  }

  numerator_label <- if (numerator == "ev") "EV" else "Price"

  fundamental_label <- paste0("\u0394 ", metric_name, " (per share)")
  valuation_label <- "Valuation Effect"

  plot_data <- data %>%
    dplyr::select(date, fundamental_growth, valuation_effect) %>%
    tidyr::pivot_longer(
      cols = c(fundamental_growth, valuation_effect),
      names_to = "component",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      component = factor(
        component,
        levels = c("fundamental_growth", "valuation_effect"),
        labels = c(fundamental_label, valuation_label)
      )
    )

  color_values <- setNames(
    c("steelblue", "darkgreen"),
    c(fundamental_label, valuation_label)
  )

  last_row <- data %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::slice(1)

  subtitle_text <- paste0(
    "Cumulative ", numerator_label, " Return: ",
    scales::percent(last_row$price_return, accuracy = 0.1), "\n",
    metric_name, " Growth: ",
    scales::percent(last_row$fundamental_growth, accuracy = 0.1), "\n",
    "Valuation Effect: ",
    scales::percent(last_row$valuation_effect, accuracy = 0.1)
  )

  date_range <- range(data$date)
  date_buffer <- as.numeric(diff(date_range)) * 0.05

  plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = value, fill = component)) +
    ggplot2::geom_area(alpha = 0.7, position = "stack") +
    ggplot2::geom_line(
      data = data,
      ggplot2::aes(x = date, y = price_return),
      color = "black",
      linewidth = 0.7,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(values = color_values) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(date_range[1], date_range[2] + date_buffer)
    ) +
    ggplot2::labs(
      title = paste0(ticker, ": ", numerator_label, " Return Decomposition"),
      subtitle = subtitle_text,
      x = NULL,
      y = paste0("Cumulative ", numerator_label, " Return"),
      fill = "",
      caption = paste0(
        "Methodology:\n",
        metric_name, " Growth = % change in ", metric_name, " per share\n",
        "Valuation Effect = ", numerator_label, " Return - ", metric_name, " Growth\n",
        "Positive effect = multiple expansion; Negative effect = multiple compression\n",
        "Start Date: ", base_date
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0),
      plot.subtitle = ggplot2::element_text(hjust = 0),
      plot.caption = ggplot2::element_text(hjust = 0, size = 8, color = "gray50")
    )
}

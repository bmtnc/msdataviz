#' Create DuPont Decomposition Over Time Chart
#'
#' Creates a stacked bar chart showing ROA and leverage effect, with ROE as line overlay.
#' The bars sum to ROE: ROA + (ROE - ROA) = ROE.
#'
#' @param data Data frame with columns: date, roe, roa
#' @param ticker Character string for the ticker symbol
#' @param income_metric_name Display name for the income metric (default: "Net Income")
#'
#' @return A ggplot2 object
#' @export
plot_dupont_over_time <- function(data, ticker, income_metric_name = "Net Income") {
  avpipeline::validate_df_cols(data, c("date", "roe", "roa"))
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  plot_data <- data %>%
    dplyr::mutate(leverage_effect = roe - roa) %>%
    tidyr::pivot_longer(
      cols = c(roa, leverage_effect),
      names_to = "component",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      component = factor(
        component,
        levels = c("roa", "leverage_effect"),
        labels = c("ROA", "Financial Leverage Effect")
      )
    )

  color_values <- c("ROA" = "#7A9DC7", "Financial Leverage Effect" = "#CC8866")

  last_row <- data %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::slice(1)

  latest_roa <- last_row$roa
  latest_equity_multiplier <- last_row$roe / last_row$roa

  subtitle_text <- paste0(
    "Latest ROA: ", scales::percent(latest_roa, accuracy = 0.1), "\n",
    "Latest Equity Multiplier: ", sprintf("%.2fx", latest_equity_multiplier)
  )

  date_range <- range(data$date)
  date_buffer <- as.numeric(diff(date_range)) * 0.08

  # Calculate bar width to fill gaps (quarterly data ~ 90 days)
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
      color = "gray50",
      linewidth = 0.2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = roe),
      color = "black",
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = last_row,
      ggplot2::aes(y = roe),
      color = "black",
      size = 3
    ) +
    ggplot2::geom_text(
      data = last_row,
      ggplot2::aes(y = roe, label = scales::percent(roe, accuracy = 0.1)),
      color = "black",
      hjust = -0.5,
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
      title = paste0(ticker, ": ROE Decomposition (DuPont)"),
      subtitle = subtitle_text,
      x = NULL,
      y = "ROE",
      fill = NULL,
      caption = paste0(
        "ROE = ", income_metric_name, " / Equity\n",
        "ROA = ", income_metric_name, " / Assets\n",
        "Financial Leverage Effect = ROE - ROA"
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0),
      plot.subtitle = ggplot2::element_text(hjust = 0),
      plot.caption = ggplot2::element_text(hjust = 0, size = 8, color = "gray50")
    )
}

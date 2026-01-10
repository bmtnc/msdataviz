#' Plot Invested Capital Decomposition
#'
#' Creates a stacked bar chart showing cumulative change in invested capital
#' decomposed into net income, dividends, debt change, and equity activity.
#'
#' @param data Data frame from calculate_ic_decomposition with columns:
#'   date, cum_net_income, cum_dividends, cum_debt_change, cum_equity_activity,
#'   cum_ic_change
#' @param ticker Character string for the ticker symbol
#' @param base_date Date object for the start of the decomposition period
#'
#' @return A ggplot2 object
#' @export
plot_ic_decomposition <- function(
    data,
    ticker,
    base_date = NULL
) {
  required_cols <- c(
    "date", "cum_net_income", "cum_dividends", "cum_debt_change",
    "cum_equity_activity", "cum_ic_change"
  )
  avpipeline::validate_df_cols(data, required_cols)
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  if (is.null(base_date)) {
    base_date <- min(data$date)
  }

  net_income_label <- "Net Income"
  dividends_label <- "Dividends"
  debt_label <- "Debt Change"
  equity_activity_label <- "Equity Activity"

  plot_data <- data %>%
    dplyr::select(
      date,
      cum_net_income,
      cum_dividends,
      cum_debt_change,
      cum_equity_activity
    ) %>%
    tidyr::pivot_longer(
      cols = c(cum_net_income, cum_dividends, cum_debt_change, cum_equity_activity),
      names_to = "component",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      component = factor(
        component,
        levels = c(
          "cum_net_income", "cum_debt_change",
          "cum_dividends", "cum_equity_activity"
        ),
        labels = c(
          net_income_label, debt_label,
          dividends_label, equity_activity_label
        )
      )
    )

  color_values <- stats::setNames(
    c("#274C77", "#D64933", "#214E34", "#69A197"),
    c(net_income_label, debt_label, dividends_label, equity_activity_label)
  )

  last_row <- data %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::slice(1)

  subtitle_text <- paste0(
    "Cumulative IC Change: ", scales::dollar(last_row$cum_ic_change / 1e9, accuracy = 0.1, suffix = "B"), "\n",
    "Net Income: ", scales::dollar(last_row$cum_net_income / 1e9, accuracy = 0.1, suffix = "B"), " | ",
    "Dividends: ", scales::dollar(last_row$cum_dividends / 1e9, accuracy = 0.1, suffix = "B"), "\n",
    "Debt Change: ", scales::dollar(last_row$cum_debt_change / 1e9, accuracy = 0.1, suffix = "B"), " | ",
    "Equity Activity: ", scales::dollar(last_row$cum_equity_activity / 1e9, accuracy = 0.1, suffix = "B")
  )

  date_range <- range(data$date)
  date_buffer <- as.numeric(diff(date_range)) * 0.08

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
      color = "#F7F3E3",
      linewidth = 0.2,
      alpha = 0.8
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = cum_ic_change, color = "IC Change"),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = last_row,
      ggplot2::aes(y = cum_ic_change),
      color = "#061826",
      size = 3
    ) +
    ggplot2::geom_text(
      data = last_row,
      ggplot2::aes(
        y = cum_ic_change,
        label = scales::dollar(cum_ic_change / 1e9, accuracy = 0.1, suffix = "B")
      ),
      color = "#061826",
      hjust = -0.3,
      size = 3.5
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = color_values) +
    ggplot2::scale_color_manual(values = c("IC Change" = "#061826")) +
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 1),
      fill = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::scale_y_continuous(labels = scales::dollar_format(scale = 1e-9, suffix = "B")) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(date_range[1] - bar_width / 2, date_range[2] + date_buffer)
    ) +
    ggplot2::labs(
      title = paste0(ticker, ": Invested Capital Decomposition"),
      subtitle = subtitle_text,
      x = NULL,
      y = "Cumulative Change ($B)",
      fill = NULL,
      color = NULL,
      caption = paste0(
        "Methodology: Net Income and Dividends are accumulated flows; Debt Change and IC Change are level differences.\n",
        "IC Change = Debt Change + Equity Change; ",
        "Equity Activity = Equity Change - Net Income - Dividends (residual: buybacks, issuances, OCI)\n",
        "Start Date: ", base_date
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0),
      plot.subtitle = ggplot2::element_text(hjust = 0),
      plot.caption = ggplot2::element_text(hjust = 0, size = 8, color = "gray50")
    )
}

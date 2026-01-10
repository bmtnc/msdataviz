#' Create DuPont Decomposition Over Time Chart
#'
#' Creates a stacked bar chart showing ROA and leverage/efficiency effect.
#' The bars sum to the return metric: ROA + (Return - ROA) = Return.
#'
#' @param data Data frame with columns: date, return_metric, roa
#' @param ticker Character string for the ticker symbol
#' @param return_label Label for the return metric (e.g., "ROE", "ROIC")
#' @param effect_label Label for the effect component (e.g., "Financial Leverage Effect")
#' @param numerator_name Display name for the numerator (e.g., "NOPAT")
#' @param denominator_name Display name for the denominator (e.g., "Equity")
#' @param multiplier_label Label for the multiplier in subtitle (e.g., "Equity Multiplier")
#' @param title_suffix Suffix for chart title (e.g., "ROE Decomposition (DuPont)")
#' @param roa_label Label for ROA component (default: "ROA", use "GROA" for gross profit)
#'
#' @return A ggplot2 object
#' @export
plot_dupont_over_time <- function(
    data,
    ticker,
    return_label,
    effect_label,
    numerator_name,
    denominator_name,
    multiplier_label,
    title_suffix,
    roa_label = "ROA"
) {
  avpipeline::validate_df_cols(data, c("date", "return_metric", "roa"))
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  plot_data <- data %>%
    dplyr::mutate(effect = return_metric - roa) %>%
    tidyr::pivot_longer(
      cols = c(roa, effect),
      names_to = "component",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      component = factor(
        component,
        levels = c("roa", "effect"),
        labels = c(roa_label, effect_label)
      )
    )

  color_values <- stats::setNames(
    c("#D5A021", "#3D405B"),
    c(roa_label, effect_label)
  )

  last_row <- data %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::slice(1)

  latest_roa <- last_row$roa
  multiplier_text <- if (latest_roa > 0.001) {
    sprintf("%.2fx", last_row$return_metric / latest_roa)
  } else {
    "N/A"
  }

  subtitle_text <- paste0(
    "Latest ", roa_label, ": ", scales::percent(latest_roa, accuracy = 0.1, big.mark = ","), "\n",
    "Latest ", multiplier_label, ": ", multiplier_text
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
      color = "#F5F9E9",
      linewidth = 0.2,
      alpha = 0.8
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = return_metric, color = return_label),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = last_row,
      ggplot2::aes(y = return_metric),
      color = "black",
      size = 3
    ) +
    ggplot2::geom_text(
      data = last_row,
      ggplot2::aes(y = return_metric, label = scales::percent(return_metric, accuracy = 0.1, big.mark = ",")),
      color = "black",
      hjust = -0.5,
      size = 3.5
    ) +
    ggplot2::scale_fill_manual(values = color_values) +
    ggplot2::scale_color_manual(values = stats::setNames("black", return_label)) +
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 1),
      fill = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(big.mark = ",")) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(date_range[1] - bar_width / 2, date_range[2] + date_buffer)
    ) +
    ggplot2::labs(
      title = paste0(ticker, ": ", title_suffix),
      subtitle = subtitle_text,
      x = NULL,
      y = return_label,
      fill = NULL,
      color = NULL,
      caption = paste0(
        return_label, " = ", numerator_name, " / ", denominator_name, "\n",
        roa_label, " = ", numerator_name, " / Assets\n",
        effect_label, " = ", return_label, " - ", roa_label
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0),
      plot.subtitle = ggplot2::element_text(hjust = 0),
      plot.caption = ggplot2::element_text(hjust = 0, size = 8, color = "gray50")
    )
}

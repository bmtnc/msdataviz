#' Plot NOPAT Decomposition
#'
#' Creates a stacked bar chart showing cumulative change in NOPAT decomposed
#' into ROIC effect (on base capital) and capital deployment effect.
#'
#' @param data Data frame from calculate_nopat_decomposition with columns:
#'   date, nopat, nopat_change, roic_effect, capital_effect
#' @param ticker Character string for the ticker symbol
#' @param metric_name Display name for the metric (default: "NOPAT")
#' @param base_date Date object for the start of the decomposition period
#'
#' @return A ggplot2 object
#' @export
plot_nopat_decomposition <- function(
    data,
    ticker,
    metric_name = "NOPAT",
    base_date = NULL
) {
  required_cols <- c("date", "nopat", "nopat_change", "roic_effect", "capital_effect")
  avpipeline::validate_df_cols(data, required_cols)
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  if (is.null(base_date)) {
    base_date <- min(data$date)
  }

  roic_effect_label <- "ROIC Effect"
  capital_effect_label <- "Capital Deployment"

  plot_data <- data %>%
    dplyr::select(date, roic_effect, capital_effect) %>%
    tidyr::pivot_longer(
      cols = c(roic_effect, capital_effect),
      names_to = "component",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      component = factor(
        component,
        levels = c("capital_effect", "roic_effect"),
        labels = c(capital_effect_label, roic_effect_label)
      )
    )

  color_values <- stats::setNames(
    c("#114B5F", "#D5A021"),
    c(capital_effect_label, roic_effect_label)
  )

  last_row <- data %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::slice(1)

  subtitle_text <- paste0(
    "Cumulative ", metric_name, " Change: ",
    scales::dollar(last_row$nopat_change / 1e9, accuracy = 0.1, suffix = "B"), "\n",
    "Capital Deployment Effect: ",
    scales::dollar(last_row$capital_effect / 1e9, accuracy = 0.1, suffix = "B"), " | ",
    "ROIC Effect: ",
    scales::dollar(last_row$roic_effect / 1e9, accuracy = 0.1, suffix = "B")
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
      color = "#FFFAFF",
      linewidth = 0.2,
      alpha = 0.8
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = nopat_change, color = paste0(metric_name, " Change")),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = last_row,
      ggplot2::aes(y = nopat_change),
      color = "#061826",
      size = 3
    ) +
    ggplot2::geom_text(
      data = last_row,
      ggplot2::aes(
        y = nopat_change,
        label = scales::dollar(nopat_change / 1e9, accuracy = 0.1, suffix = "B")
      ),
      color = "#061826",
      hjust = -0.3,
      size = 3.5
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "solid", color = "gray40", linewidth = 0.5) +
    ggplot2::scale_fill_manual(values = color_values) +
    ggplot2::scale_color_manual(
      values = stats::setNames("#061826", paste0(metric_name, " Change"))
    ) +
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
      title = paste0(ticker, ": ", metric_name, " Decomposition"),
      subtitle = subtitle_text,
      x = NULL,
      y = paste0("Cumulative ", metric_name, " Change ($B)"),
      fill = NULL,
      color = NULL,
      caption = paste0(
        "Methodology:\n",
        "Capital Deployment Effect = (IC_t - IC_0) * ROIC_t (NOPAT from incremental capital at current returns)\n",
        "ROIC Effect = IC_0 * (ROIC_t - ROIC_0) (change in returns on base capital)\n",
        "Start Date: ", base_date
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0),
      plot.subtitle = ggplot2::element_text(hjust = 0),
      plot.caption = ggplot2::element_text(hjust = 0, size = 8, color = "gray50")
    )
}

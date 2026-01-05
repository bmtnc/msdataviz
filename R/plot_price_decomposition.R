#' Create Price Decomposition Stacked Area Chart
#'
#' Creates a stacked area chart showing cumulative price or EV change decomposition.
#'
#' @param decomposition_data Data frame with decomposition columns from calculate_price_decomposition
#' @param ticker Character string for the ticker symbol
#' @param base_date Date object for the start of the decomposition period
#' @param metric_display_name Display name for the metric in labels (default: "NOPAT")
#' @param numerator Valuation numerator: "price" or "ev" (default: "price")
#' @param title Optional custom title (default: auto-generated from ticker)
#'
#' @return A ggplot2 object
#' @export
plot_price_decomposition <- function(
    decomposition_data,
    ticker,
    base_date,
    metric_display_name = "NOPAT",
    numerator = "price",
    title = NULL
) {
  required_cols <- c(
    "date", "price_change", "fundamental_contribution",
    "multiple_contribution", "price", "multiple"
  )
  avpipeline::validate_df_cols(decomposition_data, required_cols)
  avpipeline::validate_non_empty(decomposition_data, "decomposition_data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  numerator_label <- if (numerator == "ev") "EV" else "Price"

  current_data <- decomposition_data %>%
    dplyr::slice_tail(n = 1)
  if (abs(current_data$price_change) > 0.01) {
    total_change <- current_data$price_change
    fundamental_dollars <- current_data$fundamental_contribution
    valuation_dollars <- current_data$multiple_contribution

    format_amount <- function(x) {
      paste0(ifelse(x >= 0, "+", "-"), "$", round(abs(x), 1))
    }

    subtitle_text <- paste0(
      "\u0394 ", numerator_label, ": ", format_amount(total_change), "\n",
      "\u0394 ", metric_display_name, ": ", format_amount(fundamental_dollars), "\n",
      "\u0394 Valuation: ", format_amount(valuation_dollars)
    )
  } else {
    subtitle_text <- paste0("No significant ", tolower(numerator_label), " change to analyze")
  }

  # Prepare plot data
  metric_label <- paste0("\u0394 ", metric_display_name, " (per share)")
  plot_data <- decomposition_data %>%
    dplyr::select(date, fundamental_contribution, multiple_contribution) %>%
    tidyr::pivot_longer(
      cols = c(fundamental_contribution, multiple_contribution),
      names_to = "component",
      values_to = "contribution"
    ) %>%
    dplyr::mutate(
      component = dplyr::case_when(
        component == "fundamental_contribution" ~ metric_label,
        component == "multiple_contribution" ~ "\u0394 Valuation",
        TRUE ~ component
      ),
      component = factor(component, levels = c(metric_label, "\u0394 Valuation"))
    )

  # Colors and labels
  color_values <- c("steelblue", "darkgreen")
  names(color_values) <- c(metric_label, "\u0394 Valuation")

  plot_title <- if (is.null(title)) {
    paste0(ticker, ": Cumulative ", numerator_label, " Change Decomposition")
  } else {
    title
  }

  # Build plot
  p <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = contribution, fill = component)) +
    ggplot2::geom_area(alpha = 0.7, position = "stack") +
    ggplot2::geom_line(
      data = decomposition_data,
      ggplot2::aes(x = date, y = price_change),
      color = "black",
      linewidth = 0.7,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(values = color_values) +
    ggplot2::labs(
      title = plot_title,
      subtitle = subtitle_text,
      x = NULL,
      y = paste0("Cumulative ", numerator_label, " Change ($)"),
      fill = "",
      caption = paste0(
        "Methodology:\n",
        numerator_label, " = ", metric_display_name, " (per share) \u00d7 Valuation Multiple\n",
        "Measure \u0394 ", metric_display_name, " and \u0394 Valuation from start date\n",
        "\u0394 ", numerator_label, " = (\u0394 ", metric_display_name, " \u00d7 base Multiple) + (base ", metric_display_name, " \u00d7 \u0394 Multiple)\n",
        "Start Date: ", base_date
      )
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(
        min(decomposition_data$date),
        max(decomposition_data$date) + as.numeric(diff(range(decomposition_data$date))) * 0.05
      )
    )

  # Add y-axis scale
  max_change <- max(abs(decomposition_data$price_change), na.rm = TRUE)
  if (max_change > 100) {
    p <- p + ggplot2::scale_y_continuous(labels = scales::dollar_format())
  } else {
    p <- p + ggplot2::scale_y_continuous(labels = scales::dollar_format(accuracy = 0.01))
  }

  p +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0),
      plot.subtitle = ggplot2::element_text(hjust = 0),
      plot.caption = ggplot2::element_text(hjust = 0, size = 8, color = "gray50")
    )
}

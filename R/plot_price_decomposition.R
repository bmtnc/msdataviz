#' Create Price Decomposition Stacked Area Chart
#'
#' Creates a stacked area chart showing cumulative price change decomposition.
#'
#' @param decomposition_data Data frame with columns: date, price_change,
#'   nopat_growth_contribution, share_count_contribution, multiple_contribution,
#'   price, multiple
#' @param ticker Character string for the ticker symbol
#' @param metric_name Character string for the fundamental metric (e.g., "nopat_ttm_per_share")
#' @param base_date Date object for the start of the decomposition period
#' @param title Optional custom title (default: auto-generated from ticker)
#'
#' @return A ggplot2 object
#' @export
plot_price_decomposition <- function(
    decomposition_data,
    ticker,
    metric_name,
    base_date,
    title = NULL
) {
  required_cols <- c(
    "date", "price_change", "nopat_growth_contribution",
    "share_count_contribution", "multiple_contribution", "price", "multiple"
  )
  avpipeline::validate_df_cols(decomposition_data, required_cols)
  avpipeline::validate_non_empty(decomposition_data, "decomposition_data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")
  avpipeline::validate_character_scalar(metric_name, allow_empty = FALSE, name = "metric_name")

  current_data <- decomposition_data %>%
    dplyr::slice_tail(n = 1)

  # Build subtitle

  metric_short <- tolower(gsub("_ttm_per_share", "", metric_name))
  if (abs(current_data$price_change) > 0.01) {
    nopat_dollars <- current_data$nopat_growth_contribution
    share_dollars <- current_data$share_count_contribution
    valuation_dollars <- current_data$multiple_contribution

    nopat_label <- paste0(
      ifelse(nopat_dollars >= 0, paste0(metric_short, " Growth"), paste0(metric_short, " Decline")),
      ": ", ifelse(nopat_dollars >= 0, "+", "-"), "$", round(abs(nopat_dollars), 1)
    )
    share_label <- paste0(
      ifelse(share_dollars >= 0, "Buybacks", "Dilution"),
      ": ", ifelse(share_dollars >= 0, "+", "-"), "$", round(abs(share_dollars), 1)
    )
    valuation_label <- paste0(
      ifelse(valuation_dollars >= 0, "Valuation Expansion", "Valuation Compression"),
      ": ", ifelse(valuation_dollars >= 0, "+", "-"), "$", round(abs(valuation_dollars), 1)
    )

    total_change <- current_data$price_change
    direction_text <- ifelse(total_change > 0, "Gain", "Decline")
    subtitle_text <- paste0(
      "$", round(abs(total_change), 1), " Cumulative ", direction_text, " Contribution:", "\n",
      nopat_label, " | ", share_label, " | ", valuation_label
    )
  } else {
    subtitle_text <- "No significant price change to analyze"
  }

  # Prepare plot data
  metric_label <- paste("\u0394", gsub("_ttm_per_share", "", metric_name))
  plot_data <- decomposition_data %>%
    dplyr::select(date, nopat_growth_contribution, share_count_contribution, multiple_contribution) %>%
    tidyr::pivot_longer(
      cols = c(nopat_growth_contribution, share_count_contribution, multiple_contribution),
      names_to = "component",
      values_to = "contribution"
    ) %>%
    dplyr::mutate(
      component = dplyr::case_when(
        component == "nopat_growth_contribution" ~ metric_label,
        component == "share_count_contribution" ~ "\u0394 Share Count",
        component == "multiple_contribution" ~ "\u0394 Valuation",
        TRUE ~ component
      ),
      component = factor(component, levels = c(metric_label, "\u0394 Share Count", "\u0394 Valuation"))
    )

  # Colors and labels
  color_values <- c("steelblue", "lightblue", "darkgreen")
  names(color_values) <- c(metric_label, "\u0394 Share Count", "\u0394 Valuation")

  callout_text <- paste0("$", round(current_data$price, 2), "\n", round(current_data$multiple, 1), "x")
  plot_title <- if (is.null(title)) paste0(ticker, ": Cumulative Price Change Decomposition") else title

  date_range <- diff(range(decomposition_data$date))
  max_price_change <- max(decomposition_data$price_change, na.rm = TRUE)

  # Build plot
  p <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = contribution, fill = component)) +
    ggplot2::geom_area(alpha = 0.7, position = "stack") +
    ggplot2::geom_line(
      data = decomposition_data,
      ggplot2::aes(x = date, y = price_change),
      color = "black",
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = current_data,
      ggplot2::aes(x = date, y = price_change),
      color = "black",
      size = 3,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_label(
      data = current_data,
      ggplot2::aes(x = date, y = price_change, label = callout_text),
      nudge_x = as.numeric(date_range) * 0.06,
      nudge_y = max_price_change * 0.05,
      color = "black",
      fill = "white",
      alpha = 0.9,
      size = 3,
      fontface = "bold",
      lineheight = 0.9,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(values = color_values) +
    ggplot2::labs(
      title = plot_title,
      subtitle = subtitle_text,
      x = "Date",
      y = "Cumulative Price Change ($)",
      fill = "",
      caption = paste0("Methodology: Price = EPS x Valuation Multiple\nStart Date: ", base_date)
    ) +
    ggplot2::coord_cartesian(
      xlim = c(min(decomposition_data$date), max(decomposition_data$date) + as.numeric(date_range) * 0.15)
    ) +
    ggplot2::scale_x_date(date_breaks = "6 months", date_labels = "%Y") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 1),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = 11, lineheight = 1.2, hjust = 0),
      plot.caption = ggplot2::element_text(hjust = 1)
    )

  # Add y-axis scale
  max_change <- max(abs(decomposition_data$price_change), na.rm = TRUE)
  if (max_change > 100) {
    p <- p + ggplot2::scale_y_continuous(labels = scales::dollar_format())
  } else {
    p <- p + ggplot2::scale_y_continuous(labels = scales::dollar_format(accuracy = 0.01))
  }

  p
}

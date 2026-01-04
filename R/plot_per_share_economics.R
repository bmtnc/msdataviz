#' Plot Per-Share Economics with Share Count Effects
#'
#' Creates a stacked bar chart showing per-share value decomposition.
#'
#' @param per_share_data Data frame from prepare_per_share_economics_data
#' @param ticker Character string for the ticker symbol
#' @param base_date Date object for the start of analysis
#' @param metric_display_name Display name for the metric (default: "NOPAT")
#' @param title Optional custom title
#'
#' @return A ggplot2 object
#' @export
plot_per_share_economics <- function(
    per_share_data,
    ticker,
    base_date,
    metric_display_name = "NOPAT",
    title = NULL
) {
  required_cols <- c(
    "date", "total_fundamental_indexed", "per_share_indexed", "share_count_contribution"
  )
  avpipeline::validate_df_cols(per_share_data, required_cols)
  avpipeline::validate_non_empty(per_share_data, "per_share_data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  current_data <- per_share_data %>%
    dplyr::slice_tail(n = 1)

  total_growth_pct <- current_data$total_fundamental_indexed * 100
  per_share_pct <- current_data$per_share_indexed * 100
  share_effect_pct <- current_data$share_count_contribution * 100

  share_effect_label <- if (share_effect_pct >= 0) {
    paste0("Dilution: -", round(abs(share_effect_pct), 1), "%")
  } else {
    paste0("Buybacks: +", round(abs(share_effect_pct), 1), "%")
  }

  subtitle_text <- paste0(
    "Total ", metric_display_name, " Growth: ",
    ifelse(total_growth_pct >= 0, "+", ""), round(total_growth_pct, 1), "% | ",
    metric_display_name, " per Share: ",
    ifelse(per_share_pct >= 0, "+", ""), round(per_share_pct, 1), "% | ",
    share_effect_label
  )

  plot_data <- per_share_data %>%
    dplyr::mutate(
      business_growth = per_share_indexed,
      share_effect = -share_count_contribution
    ) %>%
    dplyr::select(date, business_growth, share_effect) %>%
    tidyr::pivot_longer(
      cols = c(business_growth, share_effect),
      names_to = "component",
      values_to = "contribution"
    ) %>%
    dplyr::mutate(
      component = dplyr::case_when(
        component == "business_growth" ~ "Business Growth",
        component == "share_effect" ~ "Buybacks / Dilution",
        TRUE ~ component
      ),
      component = factor(component, levels = c("Business Growth", "Buybacks / Dilution"))
    )

  color_values <- c("Business Growth" = "steelblue", "Buybacks / Dilution" = "coral")

  plot_title <- if (is.null(title)) {
    paste0(ticker, ": ", metric_display_name, " per Share Decomposition")
  } else {
    title
  }

  p <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = contribution, fill = component)) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::geom_line(
      data = per_share_data,
      ggplot2::aes(x = date, y = per_share_indexed),
      color = "black",
      linewidth = 1,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = current_data,
      ggplot2::aes(x = date, y = per_share_indexed),
      color = "black",
      size = 3,
      inherit.aes = FALSE
    ) +
    ggplot2::scale_fill_manual(values = color_values) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(
      title = plot_title,
      subtitle = subtitle_text,
      x = NULL,
      y = paste0("Cumulative Change in ", metric_display_name, " per Share"),
      fill = "",
      caption = paste0(
        "Business Growth = change in total ", metric_display_name, " attributable to per-share value\n",
        "Buybacks/Dilution = impact from changes in share count | Start Date: ", base_date
      )
    )

  p
}

#' Plot TSR Decomposition
#'
#' Creates a stacked area chart showing the three components of TSR:
#' market cap growth, dividend effect, and share count effect.
#'
#' @param data Data frame from calculate_tsr_decomposition with columns:
#'   date, tsr, market_cap_growth, dividend_effect, share_count_effect
#' @param ticker Character string for the ticker symbol
#' @param base_date Date object for the start of the decomposition period
#'
#' @return A ggplot2 object
#' @export
plot_tsr_decomposition <- function(
    data,
    ticker,
    base_date = NULL
) {
  required_cols <- c("date", "tsr", "market_cap_growth", "dividend_effect", "share_count_effect")
  avpipeline::validate_df_cols(data, required_cols)
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  if (is.null(base_date)) {
    base_date <- min(data$date)
  }

  market_cap_label <- "Market Cap Growth"
  dividend_label <- "Dividend Effect"
  share_label <- "Share Count Effect"

  plot_data <- data %>%
    dplyr::select(date, market_cap_growth, dividend_effect, share_count_effect) %>%
    tidyr::pivot_longer(
      cols = c(market_cap_growth, dividend_effect, share_count_effect),
      names_to = "component",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      component = factor(
        component,
        levels = c("market_cap_growth", "dividend_effect", "share_count_effect"),
        labels = c(market_cap_label, dividend_label, share_label)
      )
    )

  color_values <- setNames(
    c("#69747C", "#214E34", "#69A197"),
    c(market_cap_label, dividend_label, share_label)
  )

  last_row <- data %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::slice(1)

  subtitle_text <- paste0(
    "Total Shareholder Return: ", scales::percent(last_row$tsr, accuracy = 1, big.mark = ","), "\n",
    "Market Cap Growth: ", scales::percent(last_row$market_cap_growth, accuracy = 1, big.mark = ","), "\n",
    "Dividend Effect: ", scales::percent(last_row$dividend_effect, accuracy = 1, big.mark = ","), "\n",
    "Share Count Effect: ", scales::percent(last_row$share_count_effect, accuracy = 1, big.mark = ",")
  )

  date_range <- range(data$date)
  date_buffer <- as.numeric(diff(date_range)) * 0.05

  data %>%
    ggplot2::ggplot(ggplot2::aes(x = date)) +
    ggplot2::geom_area(
      data = plot_data,
      ggplot2::aes(x = date, y = value, fill = component),
      position = "stack",
      alpha = 0.8
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = tsr, color = "TSR"),
      linewidth = 0.5
    ) +
    ggplot2::geom_point(
      data = last_row,
      ggplot2::aes(y = tsr),
      color = "#061826",
      size = 3
    ) +
    ggplot2::geom_text(
      data = last_row,
      ggplot2::aes(
        y = tsr,
        label = scales::percent(tsr, accuracy = 1, big.mark = ",")
      ),
      color = "#061826",
      hjust = -0.3,
      size = 3.5
    ) +
    ggplot2::scale_fill_manual(values = color_values) +
    ggplot2::scale_color_manual(values = c("TSR" = "#2B3D41")) +
    ggplot2::guides(
      color = ggplot2::guide_legend(order = 1),
      fill = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(big.mark = ",")) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(date_range[1], date_range[2] + date_buffer)
    ) +
    ggplot2::labs(
      title = paste0(ticker, ": TSR Decomposition"),
      subtitle = subtitle_text,
      x = NULL,
      y = "Cumulative Total Shareholder Return",
      fill = "",
      color = "",
      caption = paste0(
        "Methodology:\n",
        "TSR = cumulative % change in adjusted close (price + dividends)\n",
        "Market Cap Growth = cumulative % change in split-adjusted close \u00d7 shares\n",
        "Dividend Effect = TSR - price return (contribution from reinvested dividends)\n",
        "Share Count Effect = price return - market cap growth (buybacks/dilution)\n",
        "Start Date: ", base_date
      )
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0),
      plot.subtitle = ggplot2::element_text(hjust = 0),
      plot.caption = ggplot2::element_text(hjust = 0, size = 8, color = "gray50")
    )
}

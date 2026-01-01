#' Create Drawdown from Highs Chart
#'
#' Creates an area chart showing rolling drawdown from cumulative highs.
#' Optionally shows horizontal reference lines for sector/industry median drawdowns.
#'
#' @param data Data frame with columns: date, and either price or drawdown
#' @param ticker Character string for the ticker symbol
#' @param sector_median_drawdown Median current drawdown across sector stocks
#' @param industry_median_drawdown Median current drawdown across industry stocks
#' @param sector_name Sector name for caption
#' @param industry_name Industry name for caption
#' @param n_sector_stocks Number of stocks in sector
#' @param n_industry_stocks Number of stocks in industry
#' @param min_industry_stocks Minimum stocks required to show industry line (default: 10)
#'
#' @return A ggplot2 object
#' @export
plot_drawdown <- function(
    data,
    ticker,
    sector_median_drawdown = NULL,
    industry_median_drawdown = NULL,
    sector_name = "Sector",
    industry_name = "Industry",
    n_sector_stocks = NULL,
    n_industry_stocks = NULL,
    min_industry_stocks = 10
) {
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  # Calculate drawdown if not provided
  if ("drawdown" %in% names(data)) {
    plot_data <- data
  } else {
    avpipeline::validate_df_cols(data, c("date", "price"))
    plot_data <- data %>%
      dplyr::mutate(drawdown = drawdown_from_high(price))
  }

  # Deep sunset - pinkish red, almost crimson
  sunset_orange <- "#C0392B"

  # Check if we should show industry line
  show_industry <- !is.null(industry_median_drawdown) &&
    !is.null(n_industry_stocks) &&
    n_industry_stocks >= min_industry_stocks

  # Build caption with population counts (each on separate line)
  caption_parts <- c()
  if (!is.null(sector_median_drawdown) && !is.null(n_sector_stocks)) {
    caption_parts <- c(
      caption_parts,
      paste0(
        sector_name, " current median drawdown: ",
        scales::percent(sector_median_drawdown, accuracy = 0.1),
        " (population: ", n_sector_stocks, ")"
      )
    )
  }
  if (show_industry) {
    caption_parts <- c(
      caption_parts,
      paste0(
        industry_name, " current median drawdown: ",
        scales::percent(industry_median_drawdown, accuracy = 0.1),
        " (population: ", n_industry_stocks, ")"
      )
    )
  }
  caption <- if (length(caption_parts) > 0) paste(caption_parts, collapse = "\n") else NULL

  p <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = drawdown)) +
    ggplot2::geom_area(fill = sunset_orange, alpha = 0.7) +
    ggplot2::geom_line(color = sunset_orange, linewidth = 0.5)

  # Build legend labels
  sector_legend <- paste0(sector_name, " (Current Median)")
  industry_legend <- paste0(industry_name, " (Current Median)")

  # Add sector reference line
  if (!is.null(sector_median_drawdown)) {
    p <- p +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = sector_median_drawdown, linetype = sector_legend),
        color = "gray50",
        linewidth = 0.4
      )
  }

  # Add industry reference line (same crimson as main chart)
  if (show_industry) {
    p <- p +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = industry_median_drawdown, linetype = industry_legend),
        color = sunset_orange,
        linewidth = 0.4
      )
  }

  # Build linetype scale
  linetype_values <- c()
  if (!is.null(sector_median_drawdown)) {
    linetype_values[sector_legend] <- "dashed"
  }
  if (show_industry) {
    linetype_values[industry_legend] <- "dashed"
  }

  if (length(linetype_values) > 0) {
    p <- p +
      ggplot2::scale_linetype_manual(values = linetype_values)
  }

  p +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = "Drawdown",
      linetype = NULL,
      caption = caption
    ) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(hjust = 0, size = 8, color = "gray50")
    )
}

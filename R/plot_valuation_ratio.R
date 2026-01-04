#' Create Valuation Ratio Chart
#'
#' Creates a line chart showing valuation ratio with sector, subsector, and industry medians.
#' Subsector and industry lines are only shown if sample size is sufficient (>= 10 stocks).
#'
#' @param data Data frame with columns: date, valuation_ratio, and optionally
#'   sector_valuation_ratio, subsector_valuation_ratio, and industry_valuation_ratio
#' @param ticker Character string for the ticker symbol
#' @param metric_name Metric name for y-axis label
#' @param sector_name Sector name for legend
#' @param subsector_name Subsector name for legend
#' @param industry_name Industry name for legend
#' @param n_sector_stocks Number of stocks in sector for caption
#' @param n_subsector_stocks Number of stocks in subsector for caption
#' @param n_industry_stocks Number of stocks in industry for caption
#' @param min_subsector_stocks Minimum stocks required to show subsector line (default: 10)
#' @param min_industry_stocks Minimum stocks required to show industry line (default: 10)
#'
#' @return A ggplot2 object
#' @export
plot_valuation_ratio <- function(
    data,
    ticker,
    metric_name = "EV to NOPAT",
    sector_name = "Sector",
    subsector_name = "Subsector",
    industry_name = "Industry",
    n_sector_stocks = NULL,
    n_subsector_stocks = NULL,
    n_industry_stocks = NULL,
    min_subsector_stocks = 10,
    min_industry_stocks = 10
) {
  avpipeline::validate_df_cols(data, c("date", "valuation_ratio"))
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  # Only show lines if column exists AND n_stocks is provided
  has_sector <- "sector_valuation_ratio" %in% names(data) &&
    !is.null(n_sector_stocks)
  has_subsector <- "subsector_valuation_ratio" %in% names(data) &&
    !is.null(n_subsector_stocks) &&
    n_subsector_stocks >= min_subsector_stocks
  has_industry <- "industry_valuation_ratio" %in% names(data) &&
    !is.null(n_industry_stocks) &&
    n_industry_stocks >= min_industry_stocks

  # Build caption with population counts (each on separate line)
  # Convert snake_case names to display case for labels
  sector_display <- to_display_case(sector_name)
  subsector_display <- to_display_case(subsector_name)
  industry_display <- to_display_case(industry_name)

  caption_parts <- c()
  if (!is.null(n_sector_stocks)) {
    caption_parts <- c(caption_parts, paste0(sector_display, " population: ", n_sector_stocks))
  }
  if (!is.null(n_subsector_stocks)) {
    caption_parts <- c(caption_parts, paste0(subsector_display, " population: ", n_subsector_stocks))
  }
  if (!is.null(n_industry_stocks)) {
    caption_parts <- c(caption_parts, paste0(industry_display, " population: ", n_industry_stocks))
  }
  caption <- if (length(caption_parts) > 0) paste(caption_parts, collapse = "\n") else NULL

  # Legend labels
  sector_legend <- paste0(sector_display, " (Median)")
  subsector_legend <- paste0(subsector_display, " (Median)")
  industry_legend <- paste0(industry_display, " (Median)")

  last_row <- data %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::slice(1)

  # Calculate x-axis range with 5% buffer on right
  date_range <- range(data$date)
  date_buffer <- as.numeric(diff(date_range)) * 0.05

  # Build color mapping
  color_values <- c("navy")
  color_names <- c(ticker)
  if (has_sector) {
    color_values <- c(color_values, "gray50")
    color_names <- c(color_names, sector_legend)
  }
  if (has_subsector) {
    color_values <- c(color_values, "steelblue")
    color_names <- c(color_names, subsector_legend)
  }
  if (has_industry) {
    color_values <- c(color_values, "darkgreen")
    color_names <- c(color_names, industry_legend)
  }

  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = date))

  # Sector line: thin, gray (bottom layer)
  if (has_sector) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = sector_valuation_ratio, color = sector_legend),
        linewidth = 0.3
      )
  }

  # Subsector line: thin, steelblue (middle layer)
  if (has_subsector) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = subsector_valuation_ratio, color = subsector_legend),
        linewidth = 0.5
      )
  }

  # Industry line: thin, darkgreen (layer above subsector)
  if (has_industry) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = industry_valuation_ratio, color = industry_legend),
        linewidth = 0.5
      )
  }

  # Ticker line: thickest, navy (top layer)
  p <- p +
    ggplot2::geom_line(
      ggplot2::aes(y = valuation_ratio, color = ticker),
      linewidth = 1.0
    )

  # Callouts for ticker
  p <- p +
    ggplot2::geom_point(
      data = last_row,
      ggplot2::aes(y = valuation_ratio),
      color = "navy",
      size = 3
    ) +
    ggplot2::geom_text(
      data = last_row,
      ggplot2::aes(y = valuation_ratio, label = sprintf("%.1fx", valuation_ratio)),
      color = "navy",
      hjust = -0.2,
      size = 3.5
    )

  # Callouts for sector
  if (has_sector) {
    p <- p +
      ggplot2::geom_point(
        data = last_row,
        ggplot2::aes(y = sector_valuation_ratio),
        color = "gray50",
        size = 2.5
      ) +
      ggplot2::geom_text(
        data = last_row,
        ggplot2::aes(y = sector_valuation_ratio, label = sprintf("%.1fx", sector_valuation_ratio)),
        color = "gray50",
        hjust = -0.2,
        size = 3
      )
  }

  # Callouts for subsector
  if (has_subsector) {
    p <- p +
      ggplot2::geom_point(
        data = last_row,
        ggplot2::aes(y = subsector_valuation_ratio),
        color = "steelblue",
        size = 2.5
      ) +
      ggplot2::geom_text(
        data = last_row,
        ggplot2::aes(y = subsector_valuation_ratio, label = sprintf("%.1fx", subsector_valuation_ratio)),
        color = "steelblue",
        hjust = -0.2,
        size = 3
      )
  }

  # Callouts for industry
  if (has_industry) {
    p <- p +
      ggplot2::geom_point(
        data = last_row,
        ggplot2::aes(y = industry_valuation_ratio),
        color = "darkgreen",
        size = 2.5
      ) +
      ggplot2::geom_text(
        data = last_row,
        ggplot2::aes(y = industry_valuation_ratio, label = sprintf("%.1fx", industry_valuation_ratio)),
        color = "darkgreen",
        hjust = -0.2,
        size = 3
      )
  }

  p +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "x")) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(date_range[1], date_range[2] + date_buffer)
    ) +
    ggplot2::scale_color_manual(
      values = stats::setNames(color_values, color_names)
    ) +
    ggplot2::labs(
      title = NULL,
      x = NULL,
      y = metric_name,
      color = NULL,
      caption = caption
    ) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(hjust = 0, size = 8, color = "gray50")
    )
}

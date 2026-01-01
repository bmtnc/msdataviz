#' Create ROIC Chart
#'
#' Creates a line chart showing ROIC over time with sector and industry medians.
#' Industry line is only shown if sample size is sufficient (>= 10 stocks).
#'
#' @param data Data frame with columns: date, roic, and optionally
#'   sector_roic and industry_roic
#' @param ticker Character string for the ticker symbol
#' @param sector_name Sector name for legend
#' @param industry_name Industry name for legend
#' @param n_sector_stocks Number of stocks in sector for caption
#' @param n_industry_stocks Number of stocks in industry for caption
#' @param min_industry_stocks Minimum stocks required to show industry line (default: 10)
#'
#' @return A ggplot2 object
#' @export
plot_roic <- function(
    data,
    ticker,
    sector_name = "Sector",
    industry_name = "Industry",
    n_sector_stocks = NULL,
    n_industry_stocks = NULL,
    min_industry_stocks = 10
) {
  avpipeline::validate_df_cols(data, c("date", "roic"))
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  has_sector <- "sector_roic" %in% names(data)
  # Only show industry line if we have sufficient sample size
  has_industry <- "industry_roic" %in% names(data) &&
    !is.null(n_industry_stocks) &&
    n_industry_stocks >= min_industry_stocks

  # Build caption with population counts (each on separate line)
  caption_parts <- c()
  if (!is.null(n_sector_stocks)) {
    caption_parts <- c(caption_parts, paste0(sector_name, " population: ", n_sector_stocks))
  }
  if (!is.null(n_industry_stocks)) {
    caption_parts <- c(caption_parts, paste0(industry_name, " population: ", n_industry_stocks))
  }
  caption <- if (length(caption_parts) > 0) paste(caption_parts, collapse = "\n") else NULL

  # Legend labels
  sector_legend <- paste0(sector_name, " (Median)")
  industry_legend <- paste0(industry_name, " (Median)")

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
  if (has_industry) {
    color_values <- c(color_values, "steelblue")
    color_names <- c(color_names, industry_legend)
  }

  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = date))

  # Sector line: thin, gray (bottom layer)
  if (has_sector) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = sector_roic, color = sector_legend),
        linewidth = 0.3
      )
  }

  # Industry line: thin, light blue (middle layer)
  if (has_industry) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = industry_roic, color = industry_legend),
        linewidth = 0.5
      )
  }

  # Ticker line: thickest, navy (top layer)
  p <- p +
    ggplot2::geom_line(
      ggplot2::aes(y = roic, color = ticker),
      linewidth = 1.0
    )

  # Callouts for ticker
  p <- p +
    ggplot2::geom_point(
      data = last_row,
      ggplot2::aes(y = roic),
      color = "navy",
      size = 3
    ) +
    ggplot2::geom_text(
      data = last_row,
      ggplot2::aes(y = roic, label = sprintf("%.1f%%", roic)),
      color = "navy",
      hjust = -0.2,
      size = 3.5
    )

  # Callouts for sector
  if (has_sector) {
    p <- p +
      ggplot2::geom_point(
        data = last_row,
        ggplot2::aes(y = sector_roic),
        color = "gray50",
        size = 2.5
      ) +
      ggplot2::geom_text(
        data = last_row,
        ggplot2::aes(y = sector_roic, label = sprintf("%.1f%%", sector_roic)),
        color = "gray50",
        hjust = -0.2,
        size = 3
      )
  }

  # Callouts for industry
  if (has_industry) {
    p <- p +
      ggplot2::geom_point(
        data = last_row,
        ggplot2::aes(y = industry_roic),
        color = "steelblue",
        size = 2.5
      ) +
      ggplot2::geom_text(
        data = last_row,
        ggplot2::aes(y = industry_roic, label = sprintf("%.1f%%", industry_roic)),
        color = "steelblue",
        hjust = -0.2,
        size = 3
      )
  }

  p +
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%")) +
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
      y = "ROIC (%)",
      color = NULL,
      caption = caption
    ) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(hjust = 0, size = 8, color = "gray50")
    )
}

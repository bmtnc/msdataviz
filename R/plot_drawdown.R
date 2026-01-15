#' Create Drawdown from Highs Chart
#'
#' Creates an area chart showing rolling drawdown from cumulative highs.
#' Optionally shows horizontal reference lines for sector/subsector/industry median drawdowns.
#'
#' @param data Data frame with columns: date, and either price or drawdown
#' @param ticker Character string for the ticker symbol
#' @param sector_median_drawdown Median current drawdown across sector stocks
#' @param subsector_median_drawdown Median current drawdown across subsector stocks
#' @param industry_median_drawdown Median current drawdown across industry stocks
#' @param sector_name Sector name for caption
#' @param subsector_name Subsector name for caption
#' @param industry_name Industry name for caption
#' @param n_sector_stocks Number of stocks in sector
#' @param n_subsector_stocks Number of stocks in subsector
#' @param n_industry_stocks Number of stocks in industry
#' @param min_subsector_stocks Minimum stocks required to show subsector line (default: 10)
#' @param min_industry_stocks Minimum stocks required to show industry line (default: 10)
#' @param show_anomalies Whether to overlay anomaly points (default: FALSE)
#' @param anomaly_mode Which anomaly detection to use: "both", "time_series", "cross_sectional"
#' @param anomaly_threshold Z-score threshold for time series anomaly detection (default: 2)
#' @param peer_drawdowns Data frame with peer drawdowns for cross-sectional comparison.
#'   Must have columns: date, ticker, drawdown.
#'
#' @return A ggplot2 object
#' @export
plot_drawdown <- function(
  data,
  ticker,
  sector_median_drawdown = NULL,
  subsector_median_drawdown = NULL,
  industry_median_drawdown = NULL,
  sector_name = "Sector",
  subsector_name = "Subsector",
  industry_name = "Industry",
  n_sector_stocks = NULL,
  n_subsector_stocks = NULL,
  n_industry_stocks = NULL,
  min_subsector_stocks = 10,
  min_industry_stocks = 10,
  show_anomalies = TRUE,
  anomaly_mode = c("both", "time_series", "cross_sectional"),
  anomaly_threshold = 1,
  peer_drawdowns = NULL
) {
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(
    ticker,
    allow_empty = FALSE,
    name = "ticker"
  )

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

  # Check if we should show subsector line
  show_subsector <- !is.null(subsector_median_drawdown) &&
    !is.null(n_subsector_stocks) &&
    n_subsector_stocks >= min_subsector_stocks

  # Check if we should show industry line
  show_industry <- !is.null(industry_median_drawdown) &&
    !is.null(n_industry_stocks) &&
    n_industry_stocks >= min_industry_stocks

  # Build caption with population counts (each on separate line)
  # Convert snake_case names to display case for labels
  sector_display <- to_display_case(sector_name)
  subsector_display <- to_display_case(subsector_name)
  industry_display <- to_display_case(industry_name)

  caption_parts <- c()
  if (!is.null(sector_median_drawdown) && !is.null(n_sector_stocks)) {
    caption_parts <- c(
      caption_parts,
      paste0(
        sector_display,
        " current median drawdown: ",
        scales::percent(sector_median_drawdown, accuracy = 0.1),
        " (population: ",
        n_sector_stocks,
        ")"
      )
    )
  }
  if (show_subsector) {
    caption_parts <- c(
      caption_parts,
      paste0(
        subsector_display,
        " current median drawdown: ",
        scales::percent(subsector_median_drawdown, accuracy = 0.1),
        " (population: ",
        n_subsector_stocks,
        ")"
      )
    )
  }
  if (show_industry) {
    caption_parts <- c(
      caption_parts,
      paste0(
        industry_display,
        " current median drawdown: ",
        scales::percent(industry_median_drawdown, accuracy = 0.1),
        " (population: ",
        n_industry_stocks,
        ")"
      )
    )
  }
  caption <- if (length(caption_parts) > 0) {
    paste(caption_parts, collapse = "\n")
  } else {
    NULL
  }

  p <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = drawdown)) +
    ggplot2::geom_area(fill = "#4C5760", alpha = 0.5) +
    ggplot2::geom_line(color = "#4C5760", linewidth = 0.5)

  # Add anomaly points if requested
  if (show_anomalies) {
    anomaly_mode <- match.arg(anomaly_mode)

    # Time series anomalies (vs own history)
    ts_flags <- if (anomaly_mode %in% c("both", "time_series")) {
      ts_anomaly(
        plot_data$drawdown,
        threshold = anomaly_threshold,
        direction = "low"
      )
    } else {
      rep(TRUE, nrow(plot_data))
    }

    # Cross-sectional anomalies (vs peers at each date)
    cs_flags <- if (anomaly_mode %in% c("both", "cross_sectional") && !is.null(peer_drawdowns)) {
      vapply(seq_len(nrow(plot_data)), function(i) {
        date_i <- plot_data$date[i]
        drawdown_i <- plot_data$drawdown[i]
        peers_i <- peer_drawdowns$drawdown[peer_drawdowns$date == date_i]
        if (length(peers_i) < 3) {
          return(FALSE)
        }
        cross_sectional_anomaly(drawdown_i, peers_i, threshold = anomaly_threshold, direction = "low")
      }, logical(1))
    } else {
      rep(TRUE, nrow(plot_data))
    }

    # Combine flags
    anomaly_flags <- ts_flags & cs_flags

    anomaly_data <- dplyr::filter(
      plot_data,
      anomaly_flags & !is.na(anomaly_flags)
    )
    if (nrow(anomaly_data) > 0) {
      p <- p +
        ggplot2::geom_point(
          data = anomaly_data,
          color = "#E71D36",
          size = 1
        )
    }
  }

  # Build legend labels
  sector_legend <- paste0(sector_display, " (Current Median)")
  subsector_legend <- paste0(subsector_display, " (Current Median)")
  industry_legend <- paste0(industry_display, " (Current Median)")

  # Add sector reference line
  if (!is.null(sector_median_drawdown)) {
    p <- p +
      ggplot2::geom_hline(
        ggplot2::aes(
          yintercept = sector_median_drawdown,
          linetype = sector_legend
        ),
        color = "gray50",
        linewidth = 0.4
      )
  }

  # Add subsector reference line (steelblue)
  if (show_subsector) {
    p <- p +
      ggplot2::geom_hline(
        ggplot2::aes(
          yintercept = subsector_median_drawdown,
          linetype = subsector_legend
        ),
        color = "steelblue",
        linewidth = 0.4
      )
  }

  # Add industry reference line (darkgreen)
  if (show_industry) {
    p <- p +
      ggplot2::geom_hline(
        ggplot2::aes(
          yintercept = industry_median_drawdown,
          linetype = industry_legend
        ),
        color = "darkgreen",
        linewidth = 0.4
      )
  }

  # Build linetype scale
  linetype_values <- c()
  if (!is.null(sector_median_drawdown)) {
    linetype_values[sector_legend] <- "dashed"
  }
  if (show_subsector) {
    linetype_values[subsector_legend] <- "dashed"
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
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = 8,
        color = "gray50"
      )
    )
}

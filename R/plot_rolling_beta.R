#' Plot Rolling Beta
#'
#' Creates a line chart of rolling beta over time.
#'
#' @param data Data frame with columns: date, beta
#' @param ticker Character string for the ticker symbol (used in title)
#' @param sector_name Character string for the sector name (used in subtitle)
#' @param n_sector_stocks Number of stocks in sector (for footnote)
#' @return A ggplot2 object
#' @export
plot_rolling_beta <- function(
  data,
  ticker,
  sector_name = "Sector",
  n_sector_stocks = NULL
) {
  avpipeline::validate_df_cols(data, c("date", "beta"))
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_character_scalar(
    ticker,
    allow_empty = FALSE,
    name = "ticker"
  )

  # Get latest data point for callout

  latest_point <- data %>%
    dplyr::filter(date == max(date))

  # Build footnote with 3 lines
  footnote_lines <- c(
    paste0(ticker, " Rolling Beta vs ", sector_name),
    "252-day rolling regression"
  )
  if (!is.null(n_sector_stocks)) {
    footnote_lines <- c(
      footnote_lines,
      paste0(sector_name, " population: ", n_sector_stocks)
    )
  }
  footnote <- paste(footnote_lines, collapse = "\n")

  data %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = beta)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 0.8) +
    ggplot2::geom_point(
      data = latest_point,
      color = "steelblue",
      size = 2
    ) +
    ggrepel::geom_text_repel(
      data = latest_point,
      ggplot2::aes(label = round(beta, 2)),
      nudge_x = 30,
      direction = "y",
      segment.color = NA,
      size = 3.5
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Beta",
      caption = footnote
    ) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(hjust = 0, size = 8)
    )
}

#' Create Financial Ratio Line Chart
#'
#' Creates a line chart showing one or more financial ratios over time.
#'
#' @param data Data frame with date column and one or more ratio columns
#' @param ratio_cols Character vector of column names to plot
#' @param labels Character vector of display labels for each ratio (same order as ratio_cols)
#' @param colors Character vector of colors for each line (same order as ratio_cols)
#' @param y_format Format for y-axis: "percent", "turns", or "numeric"
#' @param y_label Label for y-axis
#' @param ticker Ticker symbol for title
#' @param title_suffix Optional suffix for title (default: NULL, uses y_label)
#' @param peer_col Optional column name for peer median line
#' @param peer_label Label for peer median in legend (default: "Peer Median")
#' @param peer_color Color for peer median line (default: "gray50")
#' @param n_peers Number of peers for caption (default: NULL)
#'
#' @return A ggplot2 object
#' @export
plot_financial_ratio <- function(
    data,
    ratio_cols,
    labels,
    colors,
    y_format = "percent",
    y_label = NULL,
    ticker = NULL,
    title_suffix = NULL,
    peer_col = NULL,
    peer_label = "Peer Median",
    peer_color = "gray50",
    n_peers = NULL
) {
  required_cols <- c("date", ratio_cols)
  if (!is.null(peer_col)) {
    required_cols <- c(required_cols, peer_col)
  }
  avpipeline::validate_df_cols(data, required_cols)
  avpipeline::validate_non_empty(data, "data")

  if (length(ratio_cols) != length(labels) || length(ratio_cols) != length(colors)) {
    stop("ratio_cols, labels, and colors must have the same length")
  }

  # Check if peer median should be shown
  has_peer <- !is.null(peer_col) && peer_col %in% names(data)

  # Build title
  title <- NULL
  if (!is.null(ticker)) {
    suffix <- if (!is.null(title_suffix)) title_suffix else y_label
    title <- if (!is.null(suffix)) paste0(ticker, ": ", suffix) else ticker
  }

  # Build caption with peer count

  caption <- NULL
  if (has_peer && !is.null(n_peers)) {
    caption <- paste0(peer_label, " (n=", n_peers, ")")
  }

  # Get last row for callouts
  last_row <- data %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::slice(1)

  # Calculate x-axis range with buffer for callouts
  date_range <- range(data$date)
  date_buffer <- as.numeric(diff(date_range)) * 0.08

  # Build color mapping (include peer if present)
  color_values <- stats::setNames(colors, labels)
  if (has_peer) {
    color_values <- c(color_values, stats::setNames(peer_color, peer_label))
  }

  # Line widths: first line thicker if multiple, otherwise all same
  line_widths <- if (length(ratio_cols) > 1) {
    c(1.0, rep(0.7, length(ratio_cols) - 1))
  } else {
    1.0
  }

  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = date))

  # Add peer median line first (bottom layer, thinner)
  if (has_peer) {
    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = .data[[peer_col]], color = peer_label),
        linewidth = 0.5,
        linetype = "dashed"
      )
  }

  # Add lines and callouts for each ratio
  for (i in seq_along(ratio_cols)) {
    col <- ratio_cols[i]
    label <- labels[i]
    color <- colors[i]
    lw <- line_widths[i]

    p <- p +
      ggplot2::geom_line(
        ggplot2::aes(y = .data[[col]], color = !!label),
        linewidth = lw
      )

    # Callout point
    p <- p +
      ggplot2::geom_point(
        data = last_row,
        ggplot2::aes(y = .data[[col]]),
        color = color,
        size = 2.5
      )

    # Callout label
    last_value <- last_row[[col]]
    if (!is.na(last_value)) {
      label_text <- switch(
        y_format,
        percent = sprintf("%.1f%%", last_value * 100),
        turns = sprintf("%.1fx", last_value),
        sprintf("%.2f", last_value)
      )

      p <- p +
        ggplot2::geom_text(
          data = last_row,
          ggplot2::aes(y = .data[[col]]),
          label = label_text,
          color = color,
          hjust = -0.2,
          size = 3
        )
    }
  }

  # Add peer median callout
  if (has_peer) {
    peer_last_value <- last_row[[peer_col]]
    if (!is.na(peer_last_value)) {
      peer_label_text <- switch(
        y_format,
        percent = sprintf("%.1f%%", peer_last_value * 100),
        turns = sprintf("%.1fx", peer_last_value),
        sprintf("%.2f", peer_last_value)
      )

      p <- p +
        ggplot2::geom_point(
          data = last_row,
          ggplot2::aes(y = .data[[peer_col]]),
          color = peer_color,
          size = 2
        ) +
        ggplot2::geom_text(
          data = last_row,
          ggplot2::aes(y = .data[[peer_col]]),
          label = peer_label_text,
          color = peer_color,
          hjust = -0.2,
          size = 2.5
        )
    }
  }

  # Y-axis formatting
  y_scale <- switch(
    y_format,
    percent = ggplot2::scale_y_continuous(labels = function(x) paste0(x * 100, "%")),
    turns = ggplot2::scale_y_continuous(labels = function(x) paste0(x, "x")),
    ggplot2::scale_y_continuous()
  )

  p +
    y_scale +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      limits = c(date_range[1], date_range[2] + date_buffer)
    ) +
    ggplot2::scale_color_manual(values = color_values) +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = y_label,
      color = NULL,
      caption = caption
    )
}

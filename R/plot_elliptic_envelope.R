#' Plot Elliptic Envelope Scatter
#'
#' Creates a scatter plot with elliptic envelope for outlier detection.
#' Points are colored by group membership and outlier status.
#'
#' @param data Data frame with required columns
#' @param x_col Name of column for x-axis
#' @param y_col Name of column for y-axis
#' @param ticker_col Name of column containing ticker symbols
#' @param group_col Name of column for group membership (e.g., "industry")
#' @param target_ticker The ticker to highlight
#' @param target_group The group of the target ticker (for coloring same-group tickers)
#' @param envelope_fit Result from fit_elliptic_envelope()
#' @param x_label Label for x-axis
#' @param y_label Label for y-axis
#' @param subtitle Optional subtitle text (e.g., for disclaimers)
#' @param show_outlier_labels Whether to show ticker labels for outliers (default: TRUE)
#'
#' @return A ggplot2 object
#' @export
plot_elliptic_envelope <- function(
    data,
    x_col,
    y_col,
    ticker_col,
    group_col,
    target_ticker,
    target_group,
    envelope_fit,
    x_label = x_col,
    y_label = y_col,
    subtitle = NULL,
    show_outlier_labels = TRUE
) {
  avpipeline::validate_non_empty(data, "data")

  # Add envelope results to data, using winsorized values for plotting
  plot_data <- data %>%
    dplyr::mutate(
      x_val = envelope_fit$x_winsorized,
      y_val = envelope_fit$y_winsorized,
      ticker = .data[[ticker_col]],
      group = .data[[group_col]],
      is_outlier = envelope_fit$is_outlier,
      mahal_dist = envelope_fit$distances
    ) %>%
    dplyr::filter(!is.na(x_val) & !is.na(y_val))

  # Build caption with winsorization note
  winsorize_pct <- envelope_fit$winsorize_pct * 100
  winsorize_note <- sprintf("Data winsorized at %.0fth/%.0fth percentiles.", winsorize_pct, 100 - winsorize_pct)
  full_caption <- if (!is.null(subtitle)) {
    paste0(subtitle, "\n", winsorize_note)
  } else {
    winsorize_note
  }

  # Categorize points
  plot_data <- plot_data %>%
    dplyr::mutate(
      point_category = dplyr::case_when(
        ticker == target_ticker ~ "target",
        group == target_group & is_outlier ~ "same_group_outlier",
        group == target_group ~ "same_group",
        is_outlier ~ "other_outlier",
        TRUE ~ "other"
      )
    )

  # Generate ellipse boundary
  ellipse_df <- generate_ellipse_points(
    center = envelope_fit$center,
    cov = envelope_fit$cov,
    level = 1 - envelope_fit$contamination
  )

  # Define colors - distinct colors for industry vs sector peers
  # Industry peers: darker, more saturated
  # Sector peers (different industry): lighter, more transparent
  point_colors <- c(
    "target" = "navy",
    "same_group" = "#2C3E50",           # Dark charcoal for industry peers
    "same_group_outlier" = "#C0392B",   # Dark red for industry outliers
    "other" = "#BDC3C7",                # Light gray for sector peers
    "other_outlier" = "#F5B7B1"         # Light pink for sector outliers
  )

  # Point sizes - industry peers larger
  point_sizes <- c(
    "target" = 4.5,
    "same_group" = 3.0,
    "same_group_outlier" = 3.0,
    "other" = 1.8,
    "other_outlier" = 1.8
  )

  # Build plot
  p <- ggplot2::ggplot() +
    # Ellipse boundary
    ggplot2::geom_path(
      data = ellipse_df,
      ggplot2::aes(x = x, y = y),
      color = "gray50",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    # Other sector points (bottom layer) - very transparent
    ggplot2::geom_point(
      data = plot_data %>% dplyr::filter(point_category == "other"),
      ggplot2::aes(x = x_val, y = y_val),
      color = point_colors["other"],
      size = point_sizes["other"],
      alpha = 0.3
    ) +
    # Other sector outliers - slightly more visible but still faded
    ggplot2::geom_point(
      data = plot_data %>% dplyr::filter(point_category == "other_outlier"),
      ggplot2::aes(x = x_val, y = y_val),
      color = point_colors["other_outlier"],
      size = point_sizes["other_outlier"],
      alpha = 0.5
    ) +
    # Same industry points - fully opaque, dark
    ggplot2::geom_point(
      data = plot_data %>% dplyr::filter(point_category == "same_group"),
      ggplot2::aes(x = x_val, y = y_val),
      color = point_colors["same_group"],
      size = point_sizes["same_group"],
      alpha = 1.0
    ) +
    # Same industry outliers - fully opaque, dark red
    ggplot2::geom_point(
      data = plot_data %>% dplyr::filter(point_category == "same_group_outlier"),
      ggplot2::aes(x = x_val, y = y_val),
      color = point_colors["same_group_outlier"],
      size = point_sizes["same_group_outlier"],
      alpha = 1.0
    ) +
    # Target ticker (top layer) - fully opaque
    ggplot2::geom_point(
      data = plot_data %>% dplyr::filter(point_category == "target"),
      ggplot2::aes(x = x_val, y = y_val),
      color = point_colors["target"],
      size = point_sizes["target"],
      alpha = 1.0
    ) +
    ggplot2::labs(
      title = NULL,
      x = x_label,
      y = y_label,
      caption = full_caption
    ) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = 8,
        color = "gray50"
      )
    )

  # Add outlier labels if requested
  if (show_outlier_labels) {
    outlier_data <- plot_data %>%
      dplyr::filter(is_outlier | ticker == target_ticker)

    if (nrow(outlier_data) > 0) {
      p <- p +
        ggrepel::geom_text_repel(
          data = outlier_data,
          ggplot2::aes(x = x_val, y = y_val, label = ticker),
          size = 2.5,
          max.overlaps = 20,
          segment.size = 0.2,
          segment.color = "gray60"
        )
    }
  }

  p
}

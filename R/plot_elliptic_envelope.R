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
#' @param x_as_percent Whether to transform x-axis as percentage (multiplies by 100 and adds %)
#' @param x_pct_labels Whether to add % to x-axis labels (no transformation, for data already in %)
#' @param y_pct_labels Whether to add % to y-axis labels (no transformation, for data already in %)
#' @param n_sector_stocks Number of stocks in sector for caption
#' @param n_subsector_stocks Number of stocks in subsector for caption
#' @param sector_name Sector name for caption
#' @param subsector_name Subsector name for caption
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
    show_outlier_labels = TRUE,
    x_as_percent = FALSE,
    x_pct_labels = FALSE,
    y_pct_labels = FALSE,
    n_sector_stocks = NULL,
    n_subsector_stocks = NULL,
    sector_name = "Sector",
    subsector_name = "Subsector"
) {
  avpipeline::validate_non_empty(data, "data")

  # Add envelope results to data, using winsorized values for plotting
  # Apply percentage transformation if requested
  x_winsorized <- envelope_fit$x_winsorized
  if (x_as_percent) {
    x_winsorized <- x_winsorized * 100
  }

  plot_data <- data %>%
    dplyr::mutate(
      x_val = x_winsorized,
      y_val = envelope_fit$y_winsorized,
      ticker = .data[[ticker_col]],
      group = .data[[group_col]],
      is_outlier = envelope_fit$is_outlier,
      mahal_dist = envelope_fit$distances
    ) %>%
    dplyr::filter(!is.na(x_val) & !is.na(y_val))

  # Build caption with population counts and winsorization note
  # Convert snake_case names to display case for labels
  caption_parts <- c()
  if (!is.null(n_sector_stocks)) {
    caption_parts <- c(caption_parts, paste0(to_display_case(sector_name), " population: ", n_sector_stocks))
  }
  if (!is.null(n_subsector_stocks)) {
    caption_parts <- c(caption_parts, paste0(to_display_case(subsector_name), " population: ", n_subsector_stocks))
  }
  winsorize_pct <- envelope_fit$winsorize_pct * 100
  caption_parts <- c(
    caption_parts,
    sprintf("Data winsorized at %.0fth/%.0fth percentiles", winsorize_pct, 100 - winsorize_pct)
  )
  full_caption <- paste(caption_parts, collapse = "\n")

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

  # Apply same percentage transformation to ellipse if requested
  if (x_as_percent) {
    ellipse_df$x <- ellipse_df$x * 100
  }

  # Define colors - distinct colors for subsector vs sector peers
  point_colors <- c(
    "target" = "#FFD700",               # Bright gold for target ticker
    "same_group" = "#2C3E50",           # Dark charcoal for subsector peers
    "same_group_outlier" = "#C0392B",   # Dark red for subsector outliers
    "other" = "#BDC3C7",                # Light gray for sector peers
    "other_outlier" = "#F5B7B1"         # Light pink for sector outliers
  )

  # Point sizes - all same size
 point_size <- 2.0

  # Build plot
  p <- ggplot2::ggplot() +
    # Zero reference lines (bottom layer)
    ggplot2::geom_hline(yintercept = 0, color = "gray40", linewidth = 0.5, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, color = "gray40", linewidth = 0.5, linetype = "dashed") +
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
      size = point_size,
      alpha = 0.3
    ) +
    # Other sector outliers - slightly more visible but still faded
    ggplot2::geom_point(
      data = plot_data %>% dplyr::filter(point_category == "other_outlier"),
      ggplot2::aes(x = x_val, y = y_val),
      color = point_colors["other_outlier"],
      size = point_size,
      alpha = 0.5
    ) +
    # Same subsector points - semi-transparent
    ggplot2::geom_point(
      data = plot_data %>% dplyr::filter(point_category == "same_group"),
      ggplot2::aes(x = x_val, y = y_val),
      color = point_colors["same_group"],
      size = point_size,
      alpha = 0.7
    ) +
    # Same subsector outliers - semi-transparent
    ggplot2::geom_point(
      data = plot_data %>% dplyr::filter(point_category == "same_group_outlier"),
      ggplot2::aes(x = x_val, y = y_val),
      color = point_colors["same_group_outlier"],
      size = point_size,
      alpha = 0.7
    ) +
    # Target ticker (top layer) - fully opaque, bright gold, larger
    ggplot2::geom_point(
      data = plot_data %>% dplyr::filter(point_category == "target"),
      ggplot2::aes(x = x_val, y = y_val),
      color = point_colors["target"],
      size = 4.0,
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


  # Add outlier labels if requested - only for subsector peers that are outliers
  if (show_outlier_labels) {
    label_data <- plot_data %>%
      dplyr::filter(
        (group == target_group & is_outlier) | ticker == target_ticker
      )

    if (nrow(label_data) > 0) {
      p <- p +
        ggrepel::geom_text_repel(
          data = label_data,
          ggplot2::aes(x = x_val, y = y_val, label = ticker),
          size = 2.5,
          max.overlaps = 20,
          segment.size = 0.2,
          segment.color = "gray60"
        )
    }
  }


  # Add axis formatting with 10% intervals
  # x_as_percent: transforms data (x100) AND adds % with +/- signs
  # x_pct_labels: just adds % to existing values (no transformation)
  # y_pct_labels: just adds % to existing values
  if (x_as_percent) {
    p <- p +
      ggplot2::scale_x_continuous(
        labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%"),
        breaks = seq(-200, 400, by = 10)
      )
  } else if (x_pct_labels) {
    p <- p +
      ggplot2::scale_x_continuous(
        labels = function(x) paste0(x, "%"),
        breaks = seq(-200, 400, by = 20)
      )
  }

  if (y_pct_labels) {
    p <- p +
      ggplot2::scale_y_continuous(
        labels = function(y) paste0(y, "%"),
        breaks = seq(-200, 400, by = 10)
      )
  }

  p
}

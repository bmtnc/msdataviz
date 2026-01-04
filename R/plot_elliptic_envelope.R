#' Plot Elliptic Envelope Scatter
#'
#' Creates a scatter plot with elliptic envelope for outlier detection.
#' Points are colored by group membership and outlier status.
#'
#' @param data Data frame containing the population to plot
#' @param x_col Name of column for x-axis
#' @param y_col Name of column for y-axis
#' @param ticker_col Name of column containing ticker symbols
#' @param focus_group_col Name of column identifying the focus group within population
#' @param target_ticker The ticker to highlight
#' @param target_focus_group The value in focus_group_col to emphasize
#' @param envelope_fit Result from fit_elliptic_envelope()
#' @param x_label Label for x-axis
#' @param y_label Label for y-axis
#' @param subtitle Optional subtitle text (e.g., for disclaimers)
#' @param show_outlier_labels Whether to show ticker labels for focus group outliers (default: TRUE)
#' @param x_as_percent Whether to transform x-axis as percentage (multiplies by 100 and adds %)
#' @param x_pct_labels Whether to add % to x-axis labels (no transformation, for data already in %)
#' @param y_pct_labels Whether to add % to y-axis labels (no transformation, for data already in %)
#' @param n_population Number of stocks in the background population for caption
#' @param n_focus_group Number of stocks in the focus group for caption
#' @param population_label Label for the population (e.g., "Technology", "Software")
#' @param focus_group_label Label for the focus group (e.g., "Software", "Enterprise Software")
#'
#' @return A ggplot2 object
#' @export
plot_elliptic_envelope <- function(
    data,
    x_col,
    y_col,
    ticker_col,
    focus_group_col,
    target_ticker,
    target_focus_group,
    envelope_fit,
    x_label = x_col,
    y_label = y_col,
    subtitle = NULL,
    show_outlier_labels = TRUE,
    x_as_percent = FALSE,
    x_pct_labels = FALSE,
    y_pct_labels = FALSE,
    n_population = NULL,
    n_focus_group = NULL,
    population_label = "Population",
    focus_group_label = "Focus Group"
) {
  avpipeline::validate_non_empty(data, "data")
  avpipeline::validate_df_cols(data, c(ticker_col, focus_group_col, x_col, y_col))

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
      focus_group = .data[[focus_group_col]],
      is_outlier = envelope_fit$is_outlier,
      mahal_dist = envelope_fit$distances
    ) %>%
    dplyr::filter(!is.na(x_val) & !is.na(y_val))

  full_caption <- build_envelope_caption(
    n_population = n_population,
    n_focus_group = n_focus_group,
    population_label = population_label,
    focus_group_label = focus_group_label,
    winsorize_pct = envelope_fit$winsorize_pct
  )

  # Categorize points
  plot_data <- plot_data %>%
    dplyr::mutate(
      point_category = dplyr::case_when(
        ticker == target_ticker ~ "target",
        focus_group == target_focus_group & is_outlier ~ "focus_group_outlier",
        focus_group == target_focus_group ~ "focus_group",
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

  # Define colors - distinct colors for focus group vs population peers
  point_colors <- c(
    "target" = "#FFD700",               # Bright gold for target ticker
    "focus_group" = "#2C3E50",          # Dark charcoal for focus group peers
    "focus_group_outlier" = "#C0392B",  # Dark red for focus group outliers
    "other" = "#BDC3C7",                # Light gray for population peers
    "other_outlier" = "#F5B7B1"         # Light pink for population outliers
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
    # Focus group points - semi-transparent
    ggplot2::geom_point(
      data = plot_data %>% dplyr::filter(point_category == "focus_group"),
      ggplot2::aes(x = x_val, y = y_val),
      color = point_colors["focus_group"],
      size = point_size,
      alpha = 0.7
    ) +
    # Focus group outliers - semi-transparent
    ggplot2::geom_point(
      data = plot_data %>% dplyr::filter(point_category == "focus_group_outlier"),
      ggplot2::aes(x = x_val, y = y_val),
      color = point_colors["focus_group_outlier"],
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
      caption = if (!is.null(subtitle)) paste(full_caption, subtitle, sep = "\n") else full_caption
    ) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(
        hjust = 0,
        size = 8,
        color = "gray50"
      )
    )


  # Add outlier labels if requested - only for focus group peers that are outliers
  if (show_outlier_labels) {
    label_data <- plot_data %>%
      dplyr::filter(
        (focus_group == target_focus_group & is_outlier) | ticker == target_ticker
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

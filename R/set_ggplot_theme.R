#' Set Global ggplot2 Theme for Financial Plots
#'
#' Sets a consistent theme for all ggplot2 plots created after calling this function.
#' Based on theme_minimal() with customizations optimized for financial time series data.
#'
#' @param base_size Numeric value for base font size (default: 11)
#' @param title_size Numeric value for title font size (default: 14)
#' @param axis_title_size Numeric value for axis title font size (default: 12)
#' @param strip_text_size Numeric value for facet strip text size (default: 11)
#'
#' @return Invisibly returns the previous theme (for restoration if needed)
#' @export
set_ggplot_theme <- function(
  base_size = 11,
  title_size = 14,
  axis_title_size = 12,
  strip_text_size = 11
) {
  avpipeline::validate_positive(base_size, name = "base_size")
  avpipeline::validate_positive(title_size, name = "title_size")
  avpipeline::validate_positive(axis_title_size, name = "axis_title_size")
  avpipeline::validate_positive(strip_text_size, name = "strip_text_size")

  # Create custom theme based on theme_minimal
  financial_theme <- ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      # Plot title styling
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = title_size,
        face = "bold",
        margin = ggplot2::margin(b = 20)
      ),

      # Plot subtitle styling
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        size = base_size + 1,
        color = "grey40",
        margin = ggplot2::margin(b = 15)
      ),

      # Axis text styling
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = base_size - 1
      ),
      axis.text.y = ggplot2::element_text(size = base_size - 1),

      # Axis title styling
      axis.title = ggplot2::element_text(size = axis_title_size),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 15)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 15)),

      # Grid styling - remove vertical gridlines
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "grey90", linewidth = 0.3),

      # Facet styling
      strip.text = ggplot2::element_text(
        size = strip_text_size,
        face = "bold",
        margin = ggplot2::margin(b = 10)
      ),
      strip.background = ggplot2::element_rect(
        fill = "grey95",
        color = NA
      ),

      # Legend styling
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = base_size),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.margin = ggplot2::margin(t = 15),

      # Panel styling
      panel.border = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(1, "lines"),

      # Plot margins
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    )

  # Set the theme globally and return previous theme
  previous_theme <- ggplot2::theme_set(financial_theme)

  # Print confirmation message
  cat("Financial plot theme set successfully\n")
  cat("Theme will apply to all subsequent ggplot2 objects\n")

  # Return previous theme invisibly for potential restoration
  invisible(previous_theme)
}

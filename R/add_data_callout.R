#' Add Data Callout to Line Plot
#'
#' Adds a dot and label showing the latest value at the end of a line plot.
#'
#' @param p A ggplot2 object with a line layer
#' @param label_fn Function to format the label (default: scales::dollar)
#' @param point_size Size of the dot (default: 3)
#' @param text_size Size of the label text (default: 3.5)
#' @param nudge_x Horizontal offset for label (default: 0)
#' @param nudge_y Vertical offset for label (default: 0)
#'
#' @return A ggplot2 object with callout added
#' @export
add_data_callout <- function(
    p,
    label_fn = scales::dollar,
    point_size = 3,
    text_size = 3.5,
    nudge_x = 0,
    nudge_y = 0
) {
  line_color <- extract_line_color(p)

  plot_data <- ggplot2::ggplot_build(p)$data[[1]]
  last_point <- plot_data[which.max(plot_data$x), ]

  last_df <- data.frame(
    x = last_point$x,
    y = last_point$y
  )

  if (inherits(p$data[[1]], "Date")) {
    last_df$x <- as.Date(last_df$x, origin = "1970-01-01")
  }

  p +
    ggplot2::geom_point(
      data = last_df,
      ggplot2::aes(x = x, y = y),
      color = line_color,
      size = point_size,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = last_df,
      ggplot2::aes(x = x, y = y, label = label_fn(y)),
      color = line_color,
      size = text_size,
      hjust = -0.2,
      nudge_x = nudge_x,
      nudge_y = nudge_y,
      inherit.aes = FALSE
    )
}

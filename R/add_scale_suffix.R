#' Add appropriate y-axis scale suffix based on magnitude
#'
#' @param p ggplot object
#' @param max_value Maximum value to determine scale
#'
#' @return Modified ggplot object
#' @keywords internal
add_scale_suffix <- function(p, max_value) {
  if (max_value > 1e9) {
    p + ggplot2::scale_y_continuous(
      labels = scales::label_number(scale = 1e-9, suffix = "B")
    )
  } else if (max_value > 1e6) {
    p + ggplot2::scale_y_continuous(
      labels = scales::label_number(scale = 1e-6, suffix = "M")
    )
  } else {
    p
  }
}

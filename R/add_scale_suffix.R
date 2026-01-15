#' Add appropriate y-axis scale suffix based on magnitude
#'
#' @param p ggplot object
#' @param max_abs_value Maximum absolute value to determine scale
#'
#' @return Modified ggplot object
#' @keywords internal
add_scale_suffix <- function(p, max_abs_value) {
  max_abs_value <- abs(max_abs_value)

  if (max_abs_value > 1e9) {
    p + ggplot2::scale_y_continuous(
      labels = scales::label_number(scale = 1e-9, suffix = "B")
    )
  } else if (max_abs_value > 1e6) {
    p + ggplot2::scale_y_continuous(
      labels = scales::label_number(scale = 1e-6, suffix = "M")
    )
  } else {
    p
  }
}

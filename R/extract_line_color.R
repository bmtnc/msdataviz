#' Extract Line Color from ggplot
#'
#' Extracts the color used in the first geom_line layer of a ggplot.
#' Returns the first color from the scale if color is mapped aesthetically.
#'
#' @param p A ggplot2 object
#'
#' @return Character string with the color value
#' @keywords internal
extract_line_color <- function(p) {
  # Try to get color from built plot data first (works for both mapped and set colors)
  built <- ggplot2::ggplot_build(p)
  if (length(built$data) > 0 && "colour" %in% names(built$data[[1]])) {
    color <- built$data[[1]]$colour[1]
    if (is.character(color)) {
      return(color)
    }
  }

  # Fall back to checking layer parameters
  for (layer in p$layers) {
    if (inherits(layer$geom, "GeomLine")) {
      if (!is.null(layer$aes_params$colour) && is.character(layer$aes_params$colour)) {
        return(layer$aes_params$colour)
      }
    }
  }

  "black"
}

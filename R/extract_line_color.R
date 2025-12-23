#' Extract Line Color from ggplot
#'
#' Extracts the color used in the first geom_line layer of a ggplot.
#'
#' @param p A ggplot2 object
#'
#' @return Character string with the color value
#' @keywords internal
extract_line_color <- function(p) {
  for (layer in p$layers) {
    if (inherits(layer$geom, "GeomLine")) {
      if (!is.null(layer$aes_params$colour)) {
        return(layer$aes_params$colour)
      }
      if (!is.null(layer$geom$default_aes$colour)) {
        return(layer$geom$default_aes$colour)
      }
    }
  }
  "black"
}

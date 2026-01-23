#' Get Tickers in a Subsector
#'
#' Returns all tickers belonging to a given subsector.
#'
#' @param subsector_name Character string for the subsector name
#' @param ttm_data Data frame with columns: ticker, subsector
#' @return Character vector of ticker symbols
#' @export
#' @keywords internal
get_subsector_tickers <- function(subsector_name, ttm_data) {
  avpipeline::validate_character_scalar(subsector_name, allow_empty = FALSE, name = "subsector_name")
  avpipeline::validate_df_cols(ttm_data, c("ticker", "subsector"))

  tickers <- ttm_data %>%
    dplyr::filter(subsector == subsector_name) %>%
    dplyr::pull(ticker) %>%
    unique()

  if (length(tickers) == 0) {
    stop("No tickers found for subsector '", subsector_name, "'")
  }

  tickers
}

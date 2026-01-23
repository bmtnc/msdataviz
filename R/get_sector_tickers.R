#' Get All Tickers in a Sector
#'
#' Returns all unique tickers belonging to a given sector.
#'
#' @param sector Character string of the sector name
#' @param ttm_data Data frame with columns: ticker, sector
#' @return Character vector of ticker symbols
#' @export
#' @keywords internal
get_sector_tickers <- function(sector, ttm_data) {
  avpipeline::validate_character_scalar(sector, allow_empty = FALSE, name = "sector")
  avpipeline::validate_df_cols(ttm_data, c("ticker", "sector"))

  # Case-insensitive comparison to handle title case sector names
  tickers <- ttm_data %>%
    dplyr::filter(tolower(sector) == tolower(!!sector)) %>%
    dplyr::pull(ticker) %>%
    unique()

  if (length(tickers) == 0) {
    stop("No tickers found for sector '", sector, "'")
  }

  tickers
}

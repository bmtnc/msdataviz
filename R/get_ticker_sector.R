#' Get Sector for a Ticker
#'
#' Looks up the sector for a given ticker from TTM data.
#'
#' @param ticker Character string for the ticker symbol
#' @param ttm_data Data frame with columns: ticker, sector
#' @return Character string of the sector name
#' @keywords internal
get_ticker_sector <- function(ticker, ttm_data) {

  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")
  avpipeline::validate_df_cols(ttm_data, c("ticker", "sector"))


  sector <- ttm_data %>%
    dplyr::filter(ticker == !!ticker) %>%
    dplyr::pull(sector) %>%
    unique()

  if (length(sector) == 0) {
    stop("Ticker '", ticker, "' not found in ttm_data")
  }

  # Convert to title case (e.g., "INFORMATION TECHNOLOGY" -> "Information Technology")
  tools::toTitleCase(tolower(sector[1]))
}

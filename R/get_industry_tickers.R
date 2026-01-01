#' Get All Tickers in an Industry
#'
#' Returns all unique tickers belonging to a given industry.
#'
#' @param industry Character string of the industry name
#' @param ttm_data Data frame with columns: ticker, industry
#' @return Character vector of ticker symbols
#' @keywords internal
get_industry_tickers <- function(industry, ttm_data) {
  avpipeline::validate_character_scalar(industry, allow_empty = FALSE, name = "industry")
  avpipeline::validate_df_cols(ttm_data, c("ticker", "industry"))

  # Case-insensitive comparison to handle title case industry names
  tickers <- ttm_data %>%
    dplyr::filter(tolower(industry) == tolower(!!industry)) %>%
    dplyr::pull(ticker) %>%
    unique()

  if (length(tickers) == 0) {
    stop("No tickers found for industry '", industry, "'")
  }

  tickers
}

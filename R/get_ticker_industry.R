#' Get Industry for a Ticker
#'
#' Looks up the industry for a given ticker from TTM data.
#'
#' @param ticker Character string for the ticker symbol
#' @param ttm_data Data frame with columns: ticker, industry
#' @return Character string of the industry name (title case)
#' @keywords internal
get_ticker_industry <- function(ticker, ttm_data) {
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")
  avpipeline::validate_df_cols(ttm_data, c("ticker", "industry"))

  industry <- ttm_data %>%
    dplyr::filter(ticker == !!ticker) %>%
    dplyr::pull(industry) %>%
    unique()

  if (length(industry) == 0) {
    stop("Ticker '", ticker, "' not found in ttm_data")
  }

  # Convert to title case
  tools::toTitleCase(tolower(industry[1]))
}

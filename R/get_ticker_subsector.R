#' Get Subsector for a Ticker
#'
#' Looks up the subsector for a given ticker from TTM data.
#'
#' @param ticker Character string for the ticker symbol
#' @param ttm_data Data frame with columns: ticker, subsector
#' @return Character string of the subsector name (snake_case)
#' @keywords internal
get_ticker_subsector <- function(ticker, ttm_data) {
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")
  avpipeline::validate_df_cols(ttm_data, c("ticker", "subsector"))

  subsector <- ttm_data %>%
    dplyr::filter(ticker == !!ticker) %>%
    dplyr::pull(subsector) %>%
    unique()

  if (length(subsector) == 0) {
    stop("Ticker '", ticker, "' not found in ttm_data")
  }

  subsector[1]
}

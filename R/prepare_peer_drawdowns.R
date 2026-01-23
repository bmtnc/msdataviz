#' Prepare Peer Drawdowns for Cross-Sectional Comparison
#'
#' Calculates rolling drawdowns for peers of a target ticker.
#'
#' @param ticker Target ticker symbol
#' @param start_date Start date for filtering
#' @param end_date Optional end date for filtering
#' @param peer_group Peer grouping level: "sector", "subsector", or "industry"
#' @param price_data Data frame with ticker, date, adjusted_close columns
#' @param ttm_data Data frame with ticker classifications
#'
#' @return Data frame with columns: date, ticker, drawdown
#' @export
#' @keywords internal
prepare_peer_drawdowns <- function(
    ticker,
    start_date,
    end_date = NULL,
    peer_group = c("sector", "subsector", "industry"),
    price_data,
    ttm_data
) {
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")
  avpipeline::validate_df_cols(price_data, c("ticker", "date", "adjusted_close"))
  peer_group <- match.arg(peer_group)

  peer_tickers <- switch(peer_group,
    sector = {
      group_name <- get_ticker_sector(ticker, ttm_data)
      get_sector_tickers(group_name, ttm_data)
    },
    subsector = {
      group_name <- get_ticker_subsector(ticker, ttm_data)
      get_subsector_tickers(group_name, ttm_data)
    },
    industry = {
      group_name <- get_ticker_industry(ticker, ttm_data)
      get_industry_tickers(group_name, ttm_data)
    }
  )

  # Exclude target ticker from peers
  peer_tickers <- setdiff(peer_tickers, ticker)

  peer_data <- price_data %>%
    dplyr::filter(
      ticker %in% peer_tickers,
      !is.na(adjusted_close),
      date >= start_date
    )

  if (!is.null(end_date)) {
    peer_data <- peer_data %>%
      dplyr::filter(date <= end_date)
  }

  peer_data %>%
    dplyr::group_by(ticker) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(drawdown = drawdown_from_high(adjusted_close)) %>%
    dplyr::ungroup() %>%
    dplyr::select(date, ticker, drawdown)
}

#' Prepare Daily Valuation Multiples for All Tickers
#'
#' Calculates daily valuation multiples for all tickers in the universe.
#' Joins daily prices with forward-filled quarterly fundamentals.
#'
#' @param ttm_data TTM data frame from artifacts
#' @param price_data Daily price data from artifacts
#' @param start_date Start date for filtering
#'
#' @return Data frame with ticker, date, classifications, and valuation multiples at daily frequency
#' @export
prepare_universe_valuations_daily <- function(
    ttm_data,
    price_data,
    start_date = as.Date("2014-12-31")
) {
  # Prepare daily prices for all tickers
  daily_prices <- price_data %>%
    dplyr::filter(date >= start_date) %>%
    dplyr::select(ticker, date, price = adjusted_close) %>%
    dplyr::arrange(ticker, date)

  # Prepare quarterly fundamentals with per-share values
  quarterly_fundamentals <- ttm_data %>%
    dplyr::filter(fiscalDateEnding >= start_date) %>%
    dplyr::mutate(
      shares = commonStockSharesOutstanding,
      # Per-share fundamentals
      revenue_per_share = calculate_per_share(totalRevenue_ttm, shares),
      book_value_per_share = calculate_per_share(totalShareholderEquity, shares),
      gross_profit_per_share = calculate_per_share(grossProfit_ttm, shares),
      ebit_per_share = calculate_per_share(ebit_ttm, shares),
      earnings_per_share = calculate_per_share(netIncome_ttm, shares),
      fcf = calculate_fcf(operatingCashflow_ttm, capitalExpenditures_ttm),
      fcf_per_share = calculate_per_share(fcf, shares),
      ebitda_per_share = calculate_per_share(ebitda_ttm, shares),
      nopat = calculate_nopat(ebit_ttm, depreciationAndAmortization_ttm, depreciation_ttm),
      nopat_per_share = calculate_per_share(nopat, shares),
      # EV components per share
      debt_per_share = calculate_per_share(shortLongTermDebtTotal, shares),
      lease_obligations_per_share = calculate_per_share(capitalLeaseObligations, shares),
      cash_per_share = calculate_per_share(cashAndShortTermInvestments, shares),
      lt_investments_per_share = calculate_per_share(longTermInvestments, shares),
      # Yields
      dividend_per_share = calculate_per_share(dividendPayout_ttm, shares),
      buybacks_ttm = pmax(-dplyr::coalesce(proceedsFromRepurchaseOfEquity_ttm, 0), 0),
      buyback_per_share = calculate_per_share(buybacks_ttm, shares)
    ) %>%
    dplyr::select(
      ticker,
      date = reportedDate,
      sector,
      subsector,
      industry,
      revenue_per_share,
      book_value_per_share,
      gross_profit_per_share,
      ebit_per_share,
      earnings_per_share,
      fcf_per_share,
      ebitda_per_share,
      nopat_per_share,
      debt_per_share,
      lease_obligations_per_share,
      cash_per_share,
      lt_investments_per_share,
      dividend_per_share,
      buyback_per_share
    ) %>%
    # Floor to week to align with weekly price data
    dplyr::mutate(date = lubridate::floor_date(date, "week")) %>%
    dplyr::arrange(ticker, date)

  # Join daily prices with quarterly fundamentals, forward-fill by ticker
  daily_data <- daily_prices %>%
    dplyr::left_join(quarterly_fundamentals, by = c("ticker", "date")) %>%
    dplyr::group_by(ticker) %>%
    tidyr::fill(
      sector, subsector, industry,
      revenue_per_share, book_value_per_share, gross_profit_per_share,
      ebit_per_share, earnings_per_share, fcf_per_share,
      ebitda_per_share, nopat_per_share,
      debt_per_share, lease_obligations_per_share, cash_per_share, lt_investments_per_share,
      dividend_per_share, buyback_per_share,
      .direction = "down"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(revenue_per_share))

  # Calculate EV per share and valuation multiples
  daily_data %>%
    dplyr::mutate(
      ev_per_share = avpipeline:::calculate_enterprise_value_per_share(
        price,
        debt_per_share,
        lease_obligations_per_share,
        cash_per_share,
        lt_investments_per_share
      ),
      # Price-based multiples
      price_to_sales = calculate_price_to_sales(price, revenue_per_share),
      price_to_book = calculate_price_to_book(price, book_value_per_share),
      price_to_gross_profit = calculate_price_to_gross_profit(price, gross_profit_per_share),
      price_to_ebit = calculate_price_to_ebit(price, ebit_per_share),
      price_to_earnings = calculate_price_to_earnings(price, earnings_per_share),
      price_to_fcf = calculate_price_to_fcf(price, fcf_per_share),
      # EV-based multiples
      ev_to_ebitda = calculate_ev_to_ebitda(ev_per_share, ebitda_per_share),
      ev_to_nopat = calculate_ev_to_nopat(ev_per_share, nopat_per_share),
      # Yields
      dividend_yield = calculate_dividend_yield(dividend_per_share, price),
      buyback_yield = calculate_buyback_yield(buyback_per_share, price),
      shareholder_yield = calculate_shareholder_yield(dividend_per_share, buyback_per_share, price)
    ) %>%
    dplyr::select(
      ticker,
      date,
      sector,
      subsector,
      industry,
      price_to_sales,
      price_to_book,
      price_to_gross_profit,
      price_to_ebit,
      price_to_earnings,
      price_to_fcf,
      ev_to_ebitda,
      ev_to_nopat,
      dividend_yield,
      buyback_yield,
      shareholder_yield
    )
}

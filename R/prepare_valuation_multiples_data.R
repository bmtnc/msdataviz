#' Prepare Valuation Multiples Data
#'
#' Joins daily prices with quarterly TTM fundamentals and calculates valuation multiples.
#' Returns daily frequency data with forward-filled fundamentals.
#'
#' @param ticker Character string for the ticker symbol
#' @param price_data Daily price data with columns: ticker, date, adjusted_close
#' @param ttm_data Quarterly TTM data from artifacts
#' @param start_date Start date for filtering
#' @param end_date End date for filtering (NULL for no upper bound)
#' @return Data frame with date and all valuation multiples at daily frequency
#' @export
prepare_valuation_multiples_data <- function(
    ticker,
    price_data,
    ttm_data,
    start_date = as.Date("2017-12-31"),
    end_date = NULL
) {
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  # Filter and prepare price data
 ticker_prices <- price_data %>%
    dplyr::filter(ticker == !!ticker, date >= start_date) %>%
    dplyr::select(date, price = adjusted_close) %>%
    dplyr::arrange(date)

  if (!is.null(end_date)) {
    ticker_prices <- ticker_prices %>%
      dplyr::filter(date <= end_date)
  }

  # Prepare quarterly fundamentals with per-share values
  ticker_fundamentals <- ttm_data %>%
    dplyr::filter(ticker == !!ticker) %>%
    dplyr::mutate(
      shares = commonStockSharesOutstanding,
      # Per-share fundamentals for price-based multiples
      revenue_per_share = calculate_per_share(totalRevenue_ttm, shares),
      book_value_per_share = calculate_per_share(totalShareholderEquity, shares),
      gross_profit_per_share = calculate_per_share(grossProfit_ttm, shares),
      ebit_per_share = calculate_per_share(ebit_ttm, shares),
      earnings_per_share = calculate_per_share(netIncome_ttm, shares),
      fcf = calculate_fcf(operatingCashflow_ttm, capitalExpenditures_ttm),
      fcf_per_share = calculate_per_share(fcf, shares),
      # Per-share fundamentals for EV-based multiples
      ebitda_per_share = calculate_per_share(ebitda_ttm, shares),
      nopat = calculate_nopat(ebit_ttm, depreciationAndAmortization_ttm, depreciation_ttm),
      nopat_per_share = calculate_per_share(nopat, shares),
      # EV components per share
      debt_per_share = calculate_per_share(shortLongTermDebtTotal, shares),
      lease_obligations_per_share = calculate_per_share(capitalLeaseObligations, shares),
      cash_per_share = calculate_per_share(cashAndShortTermInvestments, shares),
      lt_investments_per_share = calculate_per_share(longTermInvestments, shares),
      # Yields - dividends and buybacks per share
      dividend_per_share = calculate_per_share(dividendPayout_ttm, shares),
      # Safe buyback extraction: only negative values are actual buybacks
      buybacks_ttm = pmax(-dplyr::coalesce(proceedsFromRepurchaseOfEquity_ttm, 0), 0),
      buyback_per_share = calculate_per_share(buybacks_ttm, shares)
    ) %>%
    dplyr::select(
      date = fiscalDateEnding,
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
    dplyr::arrange(date)

  # Join daily prices with quarterly fundamentals, forward-fill
  daily_data <- ticker_prices %>%
    dplyr::left_join(ticker_fundamentals, by = "date") %>%
    tidyr::fill(
      revenue_per_share, book_value_per_share, gross_profit_per_share,
      ebit_per_share, earnings_per_share, fcf_per_share,
      ebitda_per_share, nopat_per_share,
      debt_per_share, lease_obligations_per_share, cash_per_share, lt_investments_per_share,
      dividend_per_share, buyback_per_share,
      .direction = "down"
    ) %>%
    dplyr::filter(!is.na(revenue_per_share))

  # Calculate EV per share
  daily_data <- daily_data %>%
    dplyr::mutate(
      ev_per_share = avpipeline:::calculate_enterprise_value_per_share(
        price,
        debt_per_share,
        lease_obligations_per_share,
        cash_per_share,
        lt_investments_per_share
      )
    )

  # Calculate all valuation multiples
  daily_data %>%
    dplyr::mutate(
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
      date, price,
      price_to_sales, price_to_book, price_to_gross_profit,
      price_to_ebit, price_to_earnings, price_to_fcf,
      ev_to_ebitda, ev_to_nopat,
      dividend_yield, buyback_yield, shareholder_yield
    )
}

#' Prepare KPI Data for All Tickers
#'
#' Calculates financial ratios for all tickers in the universe.
#'
#' @param ttm_data TTM data frame from artifacts
#' @param start_date Start date for filtering
#'
#' @return Data frame with ticker, date, classifications, and all calculated ratios
#' @export
prepare_universe_kpis <- function(ttm_data, start_date = as.Date("2014-12-31")) {
  ttm_data %>%
    dplyr::filter(fiscalDateEnding >= start_date) %>%
    dplyr::arrange(ticker, fiscalDateEnding) %>%
    dplyr::group_by(ticker) %>%
    dplyr::mutate(
      # Calculate invested capital
      invested_capital = dplyr::coalesce(totalShareholderEquity, 0) +
        dplyr::coalesce(shortLongTermDebtTotal, 0),

      # Calculate NOPAT
      nopat = calculate_nopat(
        ebit_ttm,
        depreciationAndAmortization_ttm,
        depreciation_ttm
      ),

      # Calculate FCF
      fcf = calculate_fcf(operatingCashflow_ttm, capitalExpenditures_ttm),

      # Calculate average debt for cost of debt (lagged within ticker)
      prior_debt = dplyr::lag(shortLongTermDebtTotal),
      avg_debt = dplyr::coalesce(
        (shortLongTermDebtTotal + prior_debt) / 2,
        shortLongTermDebtTotal
      ),

      # Margins
      gross_margin = gross_margin(grossProfit_ttm, totalRevenue_ttm),
      operating_margin = operating_margin(operatingIncome_ttm, totalRevenue_ttm),
      net_margin = net_margin(netIncome_ttm, totalRevenue_ttm),

      # Returns
      roic = ifelse(invested_capital > 0, nopat / invested_capital, NA_real_),
      groic = groic(grossProfit_ttm, invested_capital),
      roe = ifelse(
        totalShareholderEquity > 0,
        netIncome_ttm / totalShareholderEquity,
        NA_real_
      ),

      # Efficiency
      fcf_conversion = fcf_conversion(fcf, nopat),

      # Credit metrics
      cost_of_debt = cost_of_debt(interestExpense_ttm, avg_debt),
      interest_coverage = interest_coverage(ebit_ttm, interestExpense_ttm),
      debt_to_ebitda = debt_to_ebitda(shortLongTermDebtTotal, ebitda_ttm)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      ticker,
      date = fiscalDateEnding,
      calendar_quarter_ending,
      sector,
      subsector,
      industry,
      roic,
      groic,
      roe,
      fcf_conversion,
      cost_of_debt,
      interest_coverage,
      debt_to_ebitda
    )
}

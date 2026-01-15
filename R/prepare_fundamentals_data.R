#' Prepare Fundamentals Data for Bar Charts
#'
#' Prepares quarterly financial metrics for charting.
#'
#' @param ticker Character string for the ticker symbol
#' @param ttm_data TTM data frame from artifacts
#' @param start_date Start date for filtering
#' @param end_date End date for filtering (NULL for no upper bound)
#'
#' @return Data frame with fiscalDateEnding and all fundamental metrics
#' @export
prepare_fundamentals_data <- function(ticker, ttm_data, start_date, end_date = NULL) {
  result <- ttm_data %>%
    dplyr::filter(ticker == !!ticker, fiscalDateEnding >= start_date) %>%
    dplyr::arrange(fiscalDateEnding) %>%
    dplyr::mutate(
      nopat = calculate_nopat(ebit_ttm, depreciationAndAmortization_ttm, depreciation_ttm),
      fcf = calculate_fcf(operatingCashflow_ttm, capitalExpenditures_ttm),
      buybacks = -proceedsFromRepurchaseOfEquity_ttm,
      total_capital_returned = dplyr::coalesce(dividendPayout_ttm, 0) +
        dplyr::coalesce(buybacks, 0)
    ) %>%
    dplyr::select(
      fiscalDateEnding,

      # Income Statement (TTM)
      revenue = totalRevenue_ttm,
      cost_of_revenue = costOfRevenue_ttm,
      gross_profit = grossProfit_ttm,
      sga = sellingGeneralAndAdministrative_ttm,
      rd = researchAndDevelopment_ttm,
      operating_expenses = operatingExpenses_ttm,
      operating_income = operatingIncome_ttm,
      ebit = ebit_ttm,
      ebitda = ebitda_ttm,
      nopat,
      interest_income = interestIncome_ttm,
      interest_expense = interestExpense_ttm,
      net_interest_income = netInterestIncome_ttm,
      income_before_tax = incomeBeforeTax_ttm,
      tax_expense = incomeTaxExpense_ttm,
      net_income = netIncome_ttm,
      depreciation_amortization = depreciationAndAmortization_ttm,

      # Cash Flow Statement (TTM)
      operating_cashflow = operatingCashflow_ttm,
      fcf,
      capex = capitalExpenditures_ttm,
      dividends = dividendPayout_ttm,
      buybacks,
      total_capital_returned,
      cashflow_investing = cashflowFromInvestment_ttm,
      cashflow_financing = cashflowFromFinancing_ttm,

      # Balance Sheet (point-in-time)
      total_assets = totalAssets,
      current_assets = totalCurrentAssets,
      non_current_assets = totalNonCurrentAssets,
      cash = cashAndShortTermInvestments,
      inventory = inventory,
      receivables = currentNetReceivables,
      ppe = propertyPlantEquipment,
      intangibles = intangibleAssets,
      goodwill = goodwill,
      total_liabilities = totalLiabilities,
      current_liabilities = totalCurrentLiabilities,
      non_current_liabilities = totalNonCurrentLiabilities,
      total_debt = shortLongTermDebtTotal,
      long_term_debt = longTermDebt,
      short_term_debt = shortTermDebt,
      shareholder_equity = totalShareholderEquity,
      retained_earnings = retainedEarnings,
      shares_outstanding = commonStockSharesOutstanding
    )

  if (!is.null(end_date)) {
    result <- result %>%
      dplyr::filter(fiscalDateEnding <= end_date)
  }

  result
}

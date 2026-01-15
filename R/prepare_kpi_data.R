#' Prepare KPI Data for Financial Ratio Charts
#'
#' Calculates financial ratios from fundamentals data.
#'
#' @param data Data frame from prepare_fundamentals_data
#'
#' @return Data frame with date and all calculated ratios
#' @export
prepare_kpi_data <- function(data) {
  avpipeline::validate_df_cols(data, c(
    "fiscalDateEnding", "revenue", "gross_profit", "operating_income",
    "net_income", "ebit", "ebitda", "nopat", "fcf", "interest_expense",
    "total_debt", "shareholder_equity"
  ))

  data %>%
    dplyr::arrange(fiscalDateEnding) %>%
    dplyr::mutate(
      # Calculate invested capital
      invested_capital = dplyr::coalesce(shareholder_equity, 0) +
        dplyr::coalesce(total_debt, 0),

      # Calculate average debt for cost of debt (current + prior quarter / 2)
      prior_debt = dplyr::lag(total_debt),
      avg_debt = dplyr::coalesce((total_debt + prior_debt) / 2, total_debt),

      # Margins (as decimals)
      gross_margin = gross_margin(gross_profit, revenue),
      operating_margin = operating_margin(operating_income, revenue),
      net_margin = net_margin(net_income, revenue),

      # Returns (as decimals)
      roic = ifelse(invested_capital > 0, nopat / invested_capital, NA_real_),
      groic = groic(gross_profit, invested_capital),
      roe = ifelse(shareholder_equity > 0, net_income / shareholder_equity, NA_real_),

      # Efficiency
      capital_turnover = capital_turnover(revenue, invested_capital),
      fcf_conversion = fcf_conversion(fcf, nopat),

      # Credit metrics
      cost_of_debt = cost_of_debt(interest_expense, avg_debt),
      interest_coverage = interest_coverage(ebit, interest_expense),
      debt_to_ebitda = debt_to_ebitda(total_debt, ebitda)
    ) %>%
    dplyr::select(
      date = fiscalDateEnding,
      gross_margin,
      operating_margin,
      net_margin,
      roic,
      groic,
      roe,
      capital_turnover,
      fcf_conversion,
      cost_of_debt,
      interest_coverage,
      debt_to_ebitda
    )
}

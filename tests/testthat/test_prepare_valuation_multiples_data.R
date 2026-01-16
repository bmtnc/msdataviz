test_that("prepare_valuation_multiples_data returns expected columns", {
  # Create minimal mock data
  price_data <- data.frame(
    ticker = rep("TEST", 5),
    date = as.Date("2023-01-01") + 0:4,
    adjusted_close = c(100, 101, 102, 103, 104)
  )

  ttm_data <- data.frame(
    ticker = "TEST",
    fiscalDateEnding = as.Date("2023-01-01"),
    commonStockSharesOutstanding = 1000,
    totalRevenue_ttm = 10000,
    totalShareholderEquity = 5000,
    grossProfit_ttm = 4000,
    ebit_ttm = 2000,
    netIncome_ttm = 1500,
    operatingCashflow_ttm = 1800,
    capitalExpenditures_ttm = 300,
    ebitda_ttm = 2500,
    depreciationAndAmortization_ttm = 500,
    depreciation_ttm = 400,
    shortLongTermDebtTotal = 2000,
    capitalLeaseObligations = 100,
    cashAndShortTermInvestments = 1000,
    longTermInvestments = 500,
    dividendPayout_ttm = 200,
    proceedsFromRepurchaseOfEquity_ttm = -300
  )

  result <- prepare_valuation_multiples_data(
    ticker = "TEST",
    price_data = price_data,
    ttm_data = ttm_data,
    start_date = as.Date("2023-01-01")
  )

  expected_cols <- c(
    "date", "price",
    "price_to_sales", "price_to_book", "price_to_gross_profit",
    "price_to_ebit", "price_to_earnings", "price_to_fcf",
    "ev_to_ebitda", "ev_to_nopat",
    "dividend_yield", "buyback_yield", "shareholder_yield"
  )

  expect_true(all(expected_cols %in% names(result)))
})

test_that("prepare_valuation_multiples_data forward-fills fundamentals", {
  price_data <- data.frame(
    ticker = rep("TEST", 10),
    date = as.Date("2023-01-01") + 0:9,
    adjusted_close = 100:109
  )

  ttm_data <- data.frame(
    ticker = "TEST",
    fiscalDateEnding = as.Date("2023-01-01"),
    commonStockSharesOutstanding = 1000,
    totalRevenue_ttm = 10000,
    totalShareholderEquity = 5000,
    grossProfit_ttm = 4000,
    ebit_ttm = 2000,
    netIncome_ttm = 1500,
    operatingCashflow_ttm = 1800,
    capitalExpenditures_ttm = 300,
    ebitda_ttm = 2500,
    depreciationAndAmortization_ttm = 500,
    depreciation_ttm = 400,
    shortLongTermDebtTotal = 2000,
    capitalLeaseObligations = 100,
    cashAndShortTermInvestments = 1000,
    longTermInvestments = 500,
    dividendPayout_ttm = 200,
    proceedsFromRepurchaseOfEquity_ttm = -300
  )

  result <- prepare_valuation_multiples_data(
    ticker = "TEST",
    price_data = price_data,
    ttm_data = ttm_data,
    start_date = as.Date("2023-01-01")
  )

  # All 10 days should have data (forward-filled from day 1)
  expect_equal(nrow(result), 10)
  # No NA values in P/S (fundamentals should be forward-filled)
  expect_false(any(is.na(result$price_to_sales)))
})

test_that("prepare_valuation_multiples_data handles safe buyback extraction", {
  price_data <- data.frame(
    ticker = rep("TEST", 2),
    date = as.Date(c("2023-01-01", "2023-04-01")),
    adjusted_close = c(100, 100)
  )

  # Test with positive proceeds (should be treated as 0 buybacks)
  ttm_data_positive <- data.frame(
    ticker = "TEST",
    fiscalDateEnding = as.Date("2023-01-01"),
    commonStockSharesOutstanding = 1000,
    totalRevenue_ttm = 10000,
    totalShareholderEquity = 5000,
    grossProfit_ttm = 4000,
    ebit_ttm = 2000,
    netIncome_ttm = 1500,
    operatingCashflow_ttm = 1800,
    capitalExpenditures_ttm = 300,
    ebitda_ttm = 2500,
    depreciationAndAmortization_ttm = 500,
    depreciation_ttm = 400,
    shortLongTermDebtTotal = 2000,
    capitalLeaseObligations = 100,
    cashAndShortTermInvestments = 1000,
    longTermInvestments = 500,
    dividendPayout_ttm = 200,
    proceedsFromRepurchaseOfEquity_ttm = 500  # Positive - not a buyback
  )

  result <- prepare_valuation_multiples_data(
    ticker = "TEST",
    price_data = price_data,
    ttm_data = ttm_data_positive,
    start_date = as.Date("2023-01-01")
  )

  # Buyback yield should be 0 when proceeds are positive
  expect_equal(result$buyback_yield[1], 0)
})

test_that("prepare_valuation_multiples_data validates ticker", {
  expect_error(
    prepare_valuation_multiples_data(
      ticker = "",
      price_data = data.frame(),
      ttm_data = data.frame()
    )
  )
})

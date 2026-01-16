test_that("prepare_universe_valuations_daily returns expected columns", {
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
    proceedsFromRepurchaseOfEquity_ttm = -300,
    sector = "Technology",
    subsector = "Software",
    industry = "Enterprise"
  )

  result <- prepare_universe_valuations_daily(
    ttm_data = ttm_data,
    price_data = price_data,
    start_date = as.Date("2023-01-01")
  )

  expected_cols <- c(
    "ticker", "date", "sector", "subsector", "industry",
    "price_to_sales", "price_to_book", "price_to_gross_profit",
    "price_to_ebit", "price_to_earnings", "price_to_fcf",
    "ev_to_ebitda", "ev_to_nopat",
    "dividend_yield", "buyback_yield", "shareholder_yield"
  )

  expect_true(all(expected_cols %in% names(result)))
})

test_that("prepare_universe_valuations_daily forward-fills fundamentals", {
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
    proceedsFromRepurchaseOfEquity_ttm = -300,
    sector = "Technology",
    subsector = "Software",
    industry = "Enterprise"
  )

  result <- prepare_universe_valuations_daily(
    ttm_data = ttm_data,
    price_data = price_data,
    start_date = as.Date("2023-01-01")
  )

  # All 10 days should have data (forward-filled from day 1)
  expect_equal(nrow(result), 10)
  # No NA values in P/S (fundamentals should be forward-filled)
  expect_false(any(is.na(result$price_to_sales)))
})

test_that("prepare_universe_valuations_daily works with multiple tickers", {
  price_data <- data.frame(
    ticker = c(rep("AAA", 5), rep("BBB", 5)),
    date = rep(as.Date("2023-01-01") + 0:4, 2),
    adjusted_close = c(100:104, 200:204)
  )

  ttm_data <- data.frame(
    ticker = c("AAA", "BBB"),
    fiscalDateEnding = as.Date("2023-01-01"),
    commonStockSharesOutstanding = c(1000, 2000),
    totalRevenue_ttm = c(10000, 20000),
    totalShareholderEquity = c(5000, 10000),
    grossProfit_ttm = c(4000, 8000),
    ebit_ttm = c(2000, 4000),
    netIncome_ttm = c(1500, 3000),
    operatingCashflow_ttm = c(1800, 3600),
    capitalExpenditures_ttm = c(300, 600),
    ebitda_ttm = c(2500, 5000),
    depreciationAndAmortization_ttm = c(500, 1000),
    depreciation_ttm = c(400, 800),
    shortLongTermDebtTotal = c(2000, 4000),
    capitalLeaseObligations = c(100, 200),
    cashAndShortTermInvestments = c(1000, 2000),
    longTermInvestments = c(500, 1000),
    dividendPayout_ttm = c(200, 400),
    proceedsFromRepurchaseOfEquity_ttm = c(-300, -600),
    sector = c("Tech", "Finance"),
    subsector = c("Software", "Banks"),
    industry = c("Enterprise", "Regional")
  )

  result <- prepare_universe_valuations_daily(
    ttm_data = ttm_data,
    price_data = price_data,
    start_date = as.Date("2023-01-01")
  )

  # Should have 5 days x 2 tickers = 10 rows
  expect_equal(nrow(result), 10)
  expect_equal(length(unique(result$ticker)), 2)
  expect_true("AAA" %in% result$ticker)
  expect_true("BBB" %in% result$ticker)
})

test_that("prepare_universe_valuations_daily returns daily frequency", {
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
    proceedsFromRepurchaseOfEquity_ttm = -300,
    sector = "Technology",
    subsector = "Software",
    industry = "Enterprise"
  )

  result <- prepare_universe_valuations_daily(
    ttm_data = ttm_data,
    price_data = price_data,
    start_date = as.Date("2023-01-01")
  )

  # Check that dates are consecutive (daily frequency)
  dates <- sort(unique(result$date))
  expect_equal(length(dates), 10)
  expect_equal(as.numeric(diff(dates)), rep(1, 9))
})

test_that("prepare_universe_valuations_daily valuations change with price", {
  price_data <- data.frame(
    ticker = rep("TEST", 2),
    date = as.Date(c("2023-01-01", "2023-01-02")),
    adjusted_close = c(100, 200)  # Price doubles
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
    proceedsFromRepurchaseOfEquity_ttm = -300,
    sector = "Technology",
    subsector = "Software",
    industry = "Enterprise"
  )

  result <- prepare_universe_valuations_daily(
    ttm_data = ttm_data,
    price_data = price_data,
    start_date = as.Date("2023-01-01")
  )

  # P/S should double when price doubles (since revenue is same)
  expect_equal(result$price_to_sales[2] / result$price_to_sales[1], 2)
})

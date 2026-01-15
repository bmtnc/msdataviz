test_that("prepare_kpi_data calculates all ratios", {
  test_data <- data.frame(
    fiscalDateEnding = as.Date(c("2023-03-31", "2023-06-30", "2023-09-30")),
    revenue = c(100, 110, 120),
    gross_profit = c(40, 44, 48),
    operating_income = c(20, 22, 24),
    net_income = c(15, 16.5, 18),
    ebit = c(22, 24.2, 26.4),
    ebitda = c(30, 33, 36),
    nopat = c(17, 18.7, 20.4),
    fcf = c(14, 15, 16),
    interest_expense = c(2, 2.2, 2.4),
    total_debt = c(50, 55, 60),
    shareholder_equity = c(100, 105, 110)
  )

  result <- prepare_kpi_data(test_data)

  expect_equal(nrow(result), 3)
  expect_true("date" %in% names(result))
  expect_true("gross_margin" %in% names(result))
  expect_true("operating_margin" %in% names(result))
  expect_true("net_margin" %in% names(result))
  expect_true("roic" %in% names(result))
  expect_true("groic" %in% names(result))
  expect_true("roe" %in% names(result))
  expect_true("capital_turnover" %in% names(result))
  expect_true("fcf_conversion" %in% names(result))
  expect_true("cost_of_debt" %in% names(result))
  expect_true("interest_coverage" %in% names(result))
  expect_true("debt_to_ebitda" %in% names(result))
})

test_that("prepare_kpi_data calculates margins correctly", {
  test_data <- data.frame(
    fiscalDateEnding = as.Date("2023-03-31"),
    revenue = 100,
    gross_profit = 40,
    operating_income = 20,
    net_income = 10,
    ebit = 22,
    ebitda = 30,
    nopat = 15,
    fcf = 12,
    interest_expense = 2,
    total_debt = 50,
    shareholder_equity = 100
  )

  result <- prepare_kpi_data(test_data)

  expect_equal(result$gross_margin, 0.4)
  expect_equal(result$operating_margin, 0.2)
  expect_equal(result$net_margin, 0.1)
})

test_that("prepare_kpi_data calculates avg debt for cost of debt", {
  test_data <- data.frame(
    fiscalDateEnding = as.Date(c("2023-03-31", "2023-06-30")),
    revenue = c(100, 100),
    gross_profit = c(40, 40),
    operating_income = c(20, 20),
    net_income = c(10, 10),
    ebit = c(22, 22),
    ebitda = c(30, 30),
    nopat = c(15, 15),
    fcf = c(12, 12),
    interest_expense = c(5, 5),
    total_debt = c(100, 200),
    shareholder_equity = c(100, 100)
  )

  result <- prepare_kpi_data(test_data)

  # First row: no prior debt, uses current debt
  expect_equal(result$cost_of_debt[1], 5 / 100)
  # Second row: avg of 100 and 200 = 150
  expect_equal(result$cost_of_debt[2], 5 / 150)
})

test_that("prepare_kpi_data validates required columns", {
  incomplete_data <- data.frame(
    fiscalDateEnding = as.Date("2023-03-31"),
    revenue = 100
  )

  expect_error(prepare_kpi_data(incomplete_data))
})

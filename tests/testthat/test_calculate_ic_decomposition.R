test_that("calculate_ic_decomposition returns correct structure", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31")),
    net_income = c(1000, 1200, 1100, 1300),
    dividends = c(100, 100, 100, 100),
    debt = c(5000, 5200, 5100, 5300),
    equity = c(10000, 10500, 10800, 11200)
  )


  result <- calculate_ic_decomposition(test_data)

  expect_s3_class(result, "data.frame")
  expect_named(result, c(
    "date", "cum_net_income", "cum_dividends", "cum_debt_change",
    "cum_equity_activity", "cum_ic_change"
  ))
  expect_equal(nrow(result), 4)
})

test_that("calculate_ic_decomposition calculates cumulative values correctly", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30")),
    net_income = c(100, 200, 150),
    dividends = c(10, 20, 15),
    debt = c(1000, 1100, 1050),
    equity = c(2000, 2100, 2200)
  )

  result <- calculate_ic_decomposition(test_data)

  expect_equal(result$cum_net_income, c(100, 300, 450))
  expect_equal(result$cum_dividends, c(-10, -30, -45))
  expect_equal(result$cum_debt_change, c(0, 100, 50))
})

test_that("calculate_ic_decomposition components sum to ic_change", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31")),
    net_income = c(1000, 1200, 1100, 1300),
    dividends = c(100, 150, 120, 130),
    debt = c(5000, 5200, 5100, 5300),
    equity = c(10000, 10500, 10800, 11200)
  )

  result <- calculate_ic_decomposition(test_data)

  calculated_ic <- result$cum_net_income + result$cum_dividends +
    result$cum_debt_change + result$cum_equity_activity

  expect_equal(result$cum_ic_change, calculated_ic)
})

test_that("calculate_ic_decomposition respects base_date parameter", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31")),
    net_income = c(1000, 1200, 1100, 1300),
    dividends = c(100, 100, 100, 100),
    debt = c(5000, 5200, 5100, 5300),
    equity = c(10000, 10500, 10800, 11200)
  )

  result <- calculate_ic_decomposition(test_data, base_date = as.Date("2020-06-30"))

  expect_equal(nrow(result), 3)
  expect_equal(min(result$date), as.Date("2020-06-30"))
  expect_equal(result$cum_debt_change[1], 0)
})

test_that("calculate_ic_decomposition handles NA values", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30")),
    net_income = c(100, NA, 150),
    dividends = c(10, 20, NA),
    debt = c(1000, 1100, 1050),
    equity = c(2000, 2100, 2200)
  )

  result <- calculate_ic_decomposition(test_data)

  expect_equal(result$cum_net_income, c(100, 100, 250))
  expect_equal(result$cum_dividends, c(-10, -30, -30))
})

test_that("calculate_ic_decomposition validates required columns", {
  incomplete_data <- data.frame(
    date = as.Date("2020-03-31"),
    net_income = 100
  )

  expect_error(
    calculate_ic_decomposition(incomplete_data),
    "Required columns missing"
  )
})

test_that("calculate_ic_decomposition validates non-empty data", {
  empty_data <- data.frame(
    date = as.Date(character()),
    net_income = numeric(),
    dividends = numeric(),
    debt = numeric(),
    equity = numeric()
  )

  expect_error(
    calculate_ic_decomposition(empty_data),
    "must have at least one row"
  )
})

test_that("calculate_ic_decomposition errors on invalid base_date", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30")),
    net_income = c(100, 200),
    dividends = c(10, 20),
    debt = c(1000, 1100),
    equity = c(2000, 2100)
  )

  expect_error(
    calculate_ic_decomposition(test_data, base_date = as.Date("2019-01-01")),
    "base_date not found"
  )
})

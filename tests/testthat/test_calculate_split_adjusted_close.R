test_that("calculate_split_adjusted_close adds split_adjusted_close column", {
  data <- data.frame(
    ticker = rep("AAPL", 4),
    date = as.Date("2024-01-01") + 0:3,
    close = c(100, 110, 60, 65),
    split_coefficient = c(1, 1, 2, 1)
  )

  result <- calculate_split_adjusted_close(data)

  expect_true("split_adjusted_close" %in% names(result))
  expect_equal(result$split_adjusted_close, c(50, 55, 60, 65))
})

test_that("calculate_split_adjusted_close handles multiple tickers independently", {
  data <- data.frame(
    ticker = c(rep("AAPL", 3), rep("GOOGL", 3)),
    date = rep(as.Date("2024-01-01") + 0:2, 2),
    close = c(100, 110, 55, 300, 100, 100),
    split_coefficient = c(1, 1, 2, 1, 3, 1)
  )

  result <- calculate_split_adjusted_close(data)

  # AAPL: 2-for-1 split on day 3
  # cumulative: 1, 1, 2; final = 2
  expect_equal(result$split_adjusted_close[result$ticker == "AAPL"], c(50, 55, 55))


  # GOOGL: 3-for-1 split on day 2
  # cumulative: 1, 3, 3; final = 3
  expect_equal(result$split_adjusted_close[result$ticker == "GOOGL"], c(100, 100, 100))
})

test_that("calculate_split_adjusted_close preserves existing columns", {
  data <- data.frame(
    ticker = rep("AAPL", 3),
    date = as.Date("2024-01-01") + 0:2,
    close = c(100, 105, 110),
    split_coefficient = c(1, 1, 1),
    adjusted_close = c(100, 105.5, 111),
    volume = c(1000, 1100, 1200)
  )

  result <- calculate_split_adjusted_close(data)

  expect_true(all(c("adjusted_close", "volume") %in% names(result)))
  expect_equal(result$volume, c(1000, 1100, 1200))
})

test_that("calculate_split_adjusted_close sorts by ticker and date", {
  # Input is out of order
  data <- data.frame(
    ticker = c("AAPL", "AAPL", "GOOGL", "AAPL"),
    date = as.Date(c("2024-01-03", "2024-01-01", "2024-01-01", "2024-01-02")),
    close = c(55, 100, 200, 110),
    split_coefficient = c(2, 1, 1, 1)
  )

  result <- calculate_split_adjusted_close(data)

  # Should be sorted by ticker, then date
  expect_equal(result$ticker, c("AAPL", "AAPL", "AAPL", "GOOGL"))
  expect_equal(result$date, as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-01")))
})

test_that("calculate_split_adjusted_close errors on missing columns", {
  data <- data.frame(
    ticker = "AAPL",
    date = as.Date("2024-01-01"),
    close = 100
    # missing split_coefficient
  )

  expect_error(calculate_split_adjusted_close(data))
})

test_that("calculate_split_adjusted_close errors on empty data", {
  data <- data.frame(
    ticker = character(0),
    date = as.Date(character(0)),
    close = numeric(0),
    split_coefficient = numeric(0)
  )

  expect_error(calculate_split_adjusted_close(data))
})

test_that("calculate_split_adjusted_close handles no splits correctly", {
  data <- data.frame(
    ticker = rep("AAPL", 3),
    date = as.Date("2024-01-01") + 0:2,
    close = c(100, 105, 110),
    split_coefficient = c(1, 1, 1)
  )

  result <- calculate_split_adjusted_close(data)

  # No splits means split_adjusted_close equals close
  expect_equal(result$split_adjusted_close, result$close)
})

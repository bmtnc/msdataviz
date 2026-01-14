test_that("calculate_tsr_decomposition returns correct structure", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
    adjusted_close = c(100, 120, 150),
    close = c(100, 118, 145),
    split_coefficient = c(1, 1, 1),
    shares = c(1000, 900, 800)
  )

  result <- calculate_tsr_decomposition(data)

  expect_true(is.data.frame(result))
  expect_true("tsr" %in% names(result))
  expect_true("market_cap_growth" %in% names(result))
  expect_true("dividend_effect" %in% names(result))
  expect_true("share_count_effect" %in% names(result))
  expect_true("split_adjusted_close" %in% names(result))
  expect_true("market_cap" %in% names(result))
  expect_equal(nrow(result), 3)
})

test_that("calculate_tsr_decomposition calculates TSR correctly", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01")),
    adjusted_close = c(100, 150),
    close = c(100, 140),
    split_coefficient = c(1, 1),
    shares = c(1000, 1000)
  )

  result <- calculate_tsr_decomposition(data)

  # TSR should be 50% (150/100 - 1)
  expect_equal(result$tsr[1], 0)
  expect_equal(result$tsr[2], 0.5)
})

test_that("calculate_tsr_decomposition calculates dividend effect correctly", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01")),
    adjusted_close = c(100, 150),
    close = c(100, 140),
    split_coefficient = c(1, 1),
    shares = c(1000, 1000)
  )

  result <- calculate_tsr_decomposition(data)

  # TSR = 50%, split-adjusted return = 40%
  # Dividend effect = 50% - 40% = 10%
  expect_equal(result$tsr[2], 0.5)
  expect_equal(result$split_adjusted_return[2], 0.4)
  expect_equal(result$dividend_effect[2], 0.1)
})

test_that("calculate_tsr_decomposition calculates share count effect correctly", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01")),
    adjusted_close = c(100, 150),
    close = c(100, 150),
    split_coefficient = c(1, 1),
    shares = c(1000, 800)
  )

  result <- calculate_tsr_decomposition(data)

  # No dividends: adjusted_close = close, so dividend_effect = 0
  # Split-adjusted return = 50%

  # Market cap: 100*1000 = 100000, then 150*800 = 120000
  # Market cap growth = 20%
  # Share count effect = 50% - 20% = 30%
  expect_equal(result$dividend_effect[2], 0)
  expect_equal(result$split_adjusted_return[2], 0.5)
  expect_equal(result$market_cap_growth[2], 0.2)
  expect_equal(result$share_count_effect[2], 0.3)
})

test_that("calculate_tsr_decomposition handles splits correctly", {
  # 2-for-1 split on day 2
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01")),
    adjusted_close = c(100, 110),
    close = c(200, 110),
    split_coefficient = c(1, 2),
    shares = c(500, 1000)
  )

  result <- calculate_tsr_decomposition(data)

  # Split-adjusted close: 200 * 1/2 = 100, then 110 * 2/2 = 110
  expect_equal(result$split_adjusted_close, c(100, 110))

  # Market cap: 100*500 = 50000, then 110*1000 = 110000
  # Market cap growth = 110000/50000 - 1 = 120%
  expect_equal(result$market_cap_growth[2], 1.2)
})

test_that("calculate_tsr_decomposition respects base_date", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
    adjusted_close = c(100, 120, 150),
    close = c(100, 118, 145),
    split_coefficient = c(1, 1, 1),
    shares = c(1000, 900, 800)
  )

  result <- calculate_tsr_decomposition(data, base_date = as.Date("2021-01-01"))

  # Should only have 2 rows (from 2021 onwards)
  expect_equal(nrow(result), 2)
  # First row should have 0 TSR (it's the base)
  expect_equal(result$tsr[1], 0)
  expect_equal(result$dividend_effect[1], 0)
  expect_equal(result$share_count_effect[1], 0)
})

test_that("calculate_tsr_decomposition validates required columns", {
  data <- data.frame(
    date = as.Date("2020-01-01"),
    adjusted_close = 100,
    close = 100
    # missing split_coefficient and shares
  )

  expect_error(calculate_tsr_decomposition(data))
})

test_that("calculate_tsr_decomposition validates non-empty data", {
  data <- data.frame(
    date = as.Date(character(0)),
    adjusted_close = numeric(0),
    close = numeric(0),
    split_coefficient = numeric(0),
    shares = numeric(0)
  )

  expect_error(calculate_tsr_decomposition(data))
})

test_that("calculate_tsr_decomposition components sum to TSR", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
    adjusted_close = c(100, 130, 180),
    close = c(100, 125, 170),
    split_coefficient = c(1, 1, 1),
    shares = c(1000, 850, 700)
  )

  result <- calculate_tsr_decomposition(data)

  # tsr = market_cap_growth + dividend_effect + share_count_effect for all rows
  expect_equal(
    result$tsr,
    result$market_cap_growth + result$dividend_effect + result$share_count_effect,
    tolerance = 1e-10
  )
})

test_that("calculate_tsr_decomposition with no dividends has zero dividend effect", {
  # When adjusted_close equals close (no dividend adjustment), dividend_effect = 0
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01")),
    adjusted_close = c(100, 150),
    close = c(100, 150),
    split_coefficient = c(1, 1),
    shares = c(1000, 1000)
  )

  result <- calculate_tsr_decomposition(data)

  expect_equal(result$dividend_effect[2], 0)
})

test_that("calculate_tsr_decomposition with no buybacks has zero share count effect", {
  # When shares don't change, share_count_effect = 0
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01")),
    adjusted_close = c(100, 150),
    close = c(100, 140),
    split_coefficient = c(1, 1),
    shares = c(1000, 1000)
  )

  result <- calculate_tsr_decomposition(data)

  expect_equal(result$share_count_effect[2], 0)
  # All return comes from market cap growth and dividends
  expect_equal(result$tsr[2], result$market_cap_growth[2] + result$dividend_effect[2])
})

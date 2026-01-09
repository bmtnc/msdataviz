test_that("calculate_tsr_decomposition returns correct structure", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
    adjusted_close = c(100, 120, 150),
    shares = c(1000, 900, 800)
  )

  result <- calculate_tsr_decomposition(data)

  expect_true(is.data.frame(result))
  expect_true("tsr" %in% names(result))
  expect_true("market_cap_growth" %in% names(result))
  expect_true("share_count_effect" %in% names(result))
  expect_true("market_cap" %in% names(result))
  expect_equal(nrow(result), 3)
})

test_that("calculate_tsr_decomposition calculates TSR correctly", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01")),
    adjusted_close = c(100, 150),
    shares = c(1000, 1000)
  )

  result <- calculate_tsr_decomposition(data)

  # TSR should be 50% (150/100 - 1)
  expect_equal(result$tsr[1], 0)
  expect_equal(result$tsr[2], 0.5)
})

test_that("calculate_tsr_decomposition calculates market cap correctly", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01")),
    adjusted_close = c(100, 150),
    shares = c(1000, 800)
  )

  result <- calculate_tsr_decomposition(data)

  # Market cap: 100*1000 = 100000, then 150*800 = 120000
  # Market cap growth = 120000/100000 - 1 = 0.2
  expect_equal(result$market_cap[1], 100000)
  expect_equal(result$market_cap[2], 120000)
  expect_equal(result$market_cap_growth[2], 0.2)
})

test_that("calculate_tsr_decomposition share_count_effect is residual", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01")),
    adjusted_close = c(100, 150),
    shares = c(1000, 800)
  )

  result <- calculate_tsr_decomposition(data)

  # TSR = 50%, market_cap_growth = 20%

  # share_count_effect = 50% - 20% = 30%
  expect_equal(result$tsr[2], 0.5)
  expect_equal(result$market_cap_growth[2], 0.2)
  expect_equal(result$share_count_effect[2], 0.3)
})

test_that("calculate_tsr_decomposition respects base_date", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
    adjusted_close = c(100, 120, 150),
    shares = c(1000, 900, 800)
  )

  result <- calculate_tsr_decomposition(data, base_date = as.Date("2021-01-01"))

  # Should only have 2 rows (from 2021 onwards)
  expect_equal(nrow(result), 2)
  # First row should have 0 TSR (it's the base)
  expect_equal(result$tsr[1], 0)
})

test_that("calculate_tsr_decomposition validates required columns", {
  data <- data.frame(
    date = as.Date("2020-01-01"),
    adjusted_close = 100
    # missing shares

  )

  expect_error(calculate_tsr_decomposition(data))
})

test_that("calculate_tsr_decomposition validates non-empty data", {
  data <- data.frame(
    date = as.Date(character(0)),
    adjusted_close = numeric(0),
    shares = numeric(0)
  )

  expect_error(calculate_tsr_decomposition(data))
})

test_that("calculate_tsr_decomposition components sum correctly", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
    adjusted_close = c(100, 130, 180),
    shares = c(1000, 850, 700)
  )

  result <- calculate_tsr_decomposition(data)

  # tsr = market_cap_growth + share_count_effect for all rows
  expect_equal(
    result$tsr,
    result$market_cap_growth + result$share_count_effect,
    tolerance = 1e-10
  )
})

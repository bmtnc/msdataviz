test_that("calculate_share_count_decomposition correctly decomposes buyback scenario", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01")),
    metric = c(100, 120),
    shares = c(10, 8)
  )

  result <- calculate_share_count_decomposition(data)

  expect_equal(nrow(result), 2)
  expect_true("organic_growth" %in% names(result))
  expect_true("share_effect" %in% names(result))
  expect_true("per_share_growth" %in% names(result))

  # Base row should be zero

  expect_equal(result$organic_growth[1], 0)
  expect_equal(result$share_effect[1], 0)
  expect_equal(result$per_share_growth[1], 0)

  # NOPAT grew 20%
  expect_equal(result$organic_growth[2], 0.20)
  # NOPAT per share: 10 -> 15, grew 50%
  expect_equal(result$per_share_growth[2], 0.50)
  # Share effect is the residual
  expect_equal(result$share_effect[2], 0.30)
  # Verify additivity
  expect_equal(
    result$organic_growth[2] + result$share_effect[2],
    result$per_share_growth[2]
  )
})

test_that("calculate_share_count_decomposition correctly decomposes dilution scenario", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01")),
    metric = c(100, 120),
    shares = c(10, 12.5)
  )

  result <- calculate_share_count_decomposition(data)

  # NOPAT grew 20%
  expect_equal(result$organic_growth[2], 0.20)
  # NOPAT per share: 10 -> 9.6, declined 4%
  expect_equal(result$per_share_growth[2], -0.04)
  # Share effect is negative (dilution)
  expect_equal(result$share_effect[2], -0.24)
  # Verify additivity
  expect_equal(
    result$organic_growth[2] + result$share_effect[2],
    result$per_share_growth[2]
  )
})

test_that("calculate_share_count_decomposition handles buyback offsetting decline", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01")),
    metric = c(100, 80),
    shares = c(10, 7)
  )

  result <- calculate_share_count_decomposition(data)

  # NOPAT declined 20%
  expect_equal(result$organic_growth[2], -0.20)
  # NOPAT per share: 10 -> 11.43, grew ~14.3%
  expect_equal(result$per_share_growth[2], 80 / 7 / 10 - 1, tolerance = 0.001)
  # Share effect is positive (buybacks helped)
  expect_true(result$share_effect[2] > 0)
  # Verify additivity
  expect_equal(
    result$organic_growth[2] + result$share_effect[2],
    result$per_share_growth[2]
  )
})

test_that("calculate_share_count_decomposition respects base_date parameter", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
    metric = c(100, 110, 120),
    shares = c(10, 10, 8)
  )

  result <- calculate_share_count_decomposition(data, base_date = as.Date("2021-01-01"))

  # Should only have 2 rows (from 2021 onwards)
  expect_equal(nrow(result), 2)
  expect_equal(min(result$date), as.Date("2021-01-01"))
  # Base row should be zero
  expect_equal(result$organic_growth[1], 0)
})

test_that("calculate_share_count_decomposition validates required columns", {
  bad_data <- data.frame(date = as.Date("2020-01-01"), value = 100)
  expect_error(calculate_share_count_decomposition(bad_data))
})

test_that("calculate_share_count_decomposition validates non-empty data", {
  empty_data <- data.frame(date = as.Date(character()), metric = numeric(), shares = numeric())
  expect_error(calculate_share_count_decomposition(empty_data))
})

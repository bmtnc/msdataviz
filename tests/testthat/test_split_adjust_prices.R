test_that("split_adjust_prices returns unchanged prices when no splits", {
  close <- c(100, 105, 110, 108)
  split_coefficient <- c(1, 1, 1, 1)

  result <- split_adjust_prices(close, split_coefficient)

  expect_equal(result, close)
})

test_that("split_adjust_prices correctly adjusts for 2-for-1 split", {
  # Day 1-2: pre-split prices, Day 3: split day (2-for-1), Day 4: post-split
  close <- c(100, 110, 60, 65)
  split_coefficient <- c(1, 1, 2, 1)

  result <- split_adjust_prices(close, split_coefficient)

  # Pre-split prices should be halved to match post-split basis
  # cumulative: 1, 1, 2, 2; final = 2
  # adjusted = close * cumulative / final
  expect_equal(result, c(50, 55, 60, 65))
})

test_that("split_adjust_prices correctly adjusts for 3-for-1 split", {
  close <- c(300, 100)
  split_coefficient <- c(1, 3)

  result <- split_adjust_prices(close, split_coefficient)

  # cumulative: 1, 3; final = 3
  # adjusted = c(300 * 1/3, 100 * 3/3) = c(100, 100)
  expect_equal(result, c(100, 100))
})

test_that("split_adjust_prices handles multiple splits", {
  # Two 2-for-1 splits
  close <- c(400, 200, 100)
  split_coefficient <- c(1, 2, 2)

  result <- split_adjust_prices(close, split_coefficient)

  # cumulative: 1, 2, 4; final = 4
  # adjusted = c(400 * 1/4, 200 * 2/4, 100 * 4/4) = c(100, 100, 100)
  expect_equal(result, c(100, 100, 100))
})

test_that("split_adjust_prices handles reverse split", {
  # 1-for-2 reverse split (split_coefficient = 0.5)
  close <- c(50, 100)
  split_coefficient <- c(1, 0.5)

  result <- split_adjust_prices(close, split_coefficient)

  # cumulative: 1, 0.5; final = 0.5
  # adjusted = c(50 * 1/0.5, 100 * 0.5/0.5) = c(100, 100)
  expect_equal(result, c(100, 100))
})

test_that("split_adjust_prices returns empty vector for empty input", {
  result <- split_adjust_prices(numeric(0), numeric(0))

  expect_equal(result, numeric(0))
})

test_that("split_adjust_prices errors on length mismatch", {
  expect_error(
    split_adjust_prices(c(100, 110), c(1, 1, 1)),
    "close and split_coefficient must have the same length"
  )
})

test_that("split_adjust_prices handles single observation", {
  result <- split_adjust_prices(100, 1)

  expect_equal(result, 100)
})

test_that("split_adjust_prices preserves latest price exactly", {
  # The last price should always equal the original (since final/final = 1)
  close <- c(100, 110, 55, 60, 65)
  split_coefficient <- c(1, 1, 2, 1, 1)

  result <- split_adjust_prices(close, split_coefficient)

  expect_equal(result[5], close[5])
})

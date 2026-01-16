test_that("calculate_dividend_yield calculates correctly", {
  expect_equal(calculate_dividend_yield(2, 100), 0.02)
  expect_equal(calculate_dividend_yield(5, 50), 0.10)
})

test_that("calculate_dividend_yield handles zero price", {
  expect_true(is.na(calculate_dividend_yield(2, 0)))
})

test_that("calculate_dividend_yield handles negative price", {
  expect_true(is.na(calculate_dividend_yield(2, -100)))
})

test_that("calculate_dividend_yield handles zero dividend", {
  expect_equal(calculate_dividend_yield(0, 100), 0)
})

test_that("calculate_dividend_yield is vectorized", {
  result <- calculate_dividend_yield(c(2, 5, 2), c(100, 50, 0))
  expect_equal(result[1:2], c(0.02, 0.10))
  expect_true(is.na(result[3]))
})

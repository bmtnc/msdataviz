test_that("calculate_shareholder_yield calculates correctly", {
  expect_equal(calculate_shareholder_yield(2, 3, 100), 0.05)
  expect_equal(calculate_shareholder_yield(5, 10, 50), 0.30)
})

test_that("calculate_shareholder_yield handles zero price", {
  expect_true(is.na(calculate_shareholder_yield(2, 3, 0)))
})

test_that("calculate_shareholder_yield handles negative price", {
  expect_true(is.na(calculate_shareholder_yield(2, 3, -100)))
})

test_that("calculate_shareholder_yield handles zero dividends and buybacks", {
  expect_equal(calculate_shareholder_yield(0, 0, 100), 0)
})

test_that("calculate_shareholder_yield is additive", {
  div_yield <- calculate_dividend_yield(2, 100)
  buyback_yield <- calculate_buyback_yield(3, 100)
  shareholder_yield <- calculate_shareholder_yield(2, 3, 100)
  expect_equal(shareholder_yield, div_yield + buyback_yield)
})

test_that("calculate_shareholder_yield is vectorized", {
  result <- calculate_shareholder_yield(c(2, 5, 2), c(3, 10, 3), c(100, 50, 0))
  expect_equal(result[1:2], c(0.05, 0.30))
  expect_true(is.na(result[3]))
})

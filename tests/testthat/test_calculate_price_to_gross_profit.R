test_that("calculate_price_to_gross_profit calculates correctly", {
  expect_equal(calculate_price_to_gross_profit(100, 20), 5)
  expect_equal(calculate_price_to_gross_profit(50, 10), 5)
})

test_that("calculate_price_to_gross_profit handles zero gross profit", {
  expect_true(is.na(calculate_price_to_gross_profit(100, 0)))
})

test_that("calculate_price_to_gross_profit handles negative gross profit", {
  expect_true(is.na(calculate_price_to_gross_profit(100, -20)))
})

test_that("calculate_price_to_gross_profit is vectorized", {
  result <- calculate_price_to_gross_profit(c(100, 50, 100), c(20, 10, 0))
  expect_equal(result[1:2], c(5, 5))
  expect_true(is.na(result[3]))
})

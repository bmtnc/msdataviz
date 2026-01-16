test_that("calculate_price_to_sales calculates correctly", {
  expect_equal(calculate_price_to_sales(100, 10), 10)
  expect_equal(calculate_price_to_sales(50, 25), 2)
})

test_that("calculate_price_to_sales handles zero revenue", {
  expect_true(is.na(calculate_price_to_sales(100, 0)))
})

test_that("calculate_price_to_sales handles negative revenue", {
  expect_true(is.na(calculate_price_to_sales(100, -10)))
})

test_that("calculate_price_to_sales is vectorized", {
  result <- calculate_price_to_sales(c(100, 50, 100), c(10, 25, 0))
  expect_equal(result[1:2], c(10, 2))
  expect_true(is.na(result[3]))
})

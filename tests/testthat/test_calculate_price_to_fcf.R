test_that("calculate_price_to_fcf calculates correctly", {
  expect_equal(calculate_price_to_fcf(100, 5), 20)
  expect_equal(calculate_price_to_fcf(200, 10), 20)
})

test_that("calculate_price_to_fcf handles zero fcf", {
  expect_true(is.na(calculate_price_to_fcf(100, 0)))
})

test_that("calculate_price_to_fcf handles negative fcf", {
  expect_true(is.na(calculate_price_to_fcf(100, -5)))
})

test_that("calculate_price_to_fcf is vectorized", {
  result <- calculate_price_to_fcf(c(100, 200, 100), c(5, 10, 0))
  expect_equal(result[1:2], c(20, 20))
  expect_true(is.na(result[3]))
})

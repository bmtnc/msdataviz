test_that("calculate_price_to_earnings calculates correctly", {
  expect_equal(calculate_price_to_earnings(100, 5), 20)
  expect_equal(calculate_price_to_earnings(50, 2.5), 20)
})

test_that("calculate_price_to_earnings handles zero earnings", {
  expect_true(is.na(calculate_price_to_earnings(100, 0)))
})

test_that("calculate_price_to_earnings handles negative earnings", {
  expect_true(is.na(calculate_price_to_earnings(100, -5)))
})

test_that("calculate_price_to_earnings is vectorized", {
  result <- calculate_price_to_earnings(c(100, 50, 100), c(5, 2.5, 0))
  expect_equal(result[1:2], c(20, 20))
  expect_true(is.na(result[3]))
})

test_that("calculate_price_to_ebit calculates correctly", {
  expect_equal(calculate_price_to_ebit(100, 10), 10)
  expect_equal(calculate_price_to_ebit(150, 15), 10)
})

test_that("calculate_price_to_ebit handles zero ebit", {
  expect_true(is.na(calculate_price_to_ebit(100, 0)))
})

test_that("calculate_price_to_ebit handles negative ebit", {
  expect_true(is.na(calculate_price_to_ebit(100, -10)))
})

test_that("calculate_price_to_ebit is vectorized", {
  result <- calculate_price_to_ebit(c(100, 150, 100), c(10, 15, 0))
  expect_equal(result[1:2], c(10, 10))
  expect_true(is.na(result[3]))
})

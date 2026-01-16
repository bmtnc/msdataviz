test_that("calculate_buyback_yield calculates correctly", {
  expect_equal(calculate_buyback_yield(3, 100), 0.03)
  expect_equal(calculate_buyback_yield(10, 50), 0.20)
})

test_that("calculate_buyback_yield handles zero price", {
  expect_true(is.na(calculate_buyback_yield(3, 0)))
})

test_that("calculate_buyback_yield handles negative price", {
  expect_true(is.na(calculate_buyback_yield(3, -100)))
})

test_that("calculate_buyback_yield handles zero buyback", {
  expect_equal(calculate_buyback_yield(0, 100), 0)
})

test_that("calculate_buyback_yield is vectorized", {
  result <- calculate_buyback_yield(c(3, 10, 3), c(100, 50, 0))
  expect_equal(result[1:2], c(0.03, 0.20))
  expect_true(is.na(result[3]))
})

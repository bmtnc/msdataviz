test_that("calculate_per_share calculates correctly", {
  expect_equal(calculate_per_share(100, 10), 10)
  expect_equal(calculate_per_share(200, 50), 4)
})

test_that("calculate_per_share handles zero shares", {
  expect_true(is.na(calculate_per_share(100, 0)))
})

test_that("calculate_per_share handles negative shares", {
  expect_true(is.na(calculate_per_share(100, -10)))
})

test_that("calculate_per_share is vectorized", {
  result <- calculate_per_share(c(100, 200, 100), c(10, 50, 0))
  expect_equal(result[1:2], c(10, 4))
  expect_true(is.na(result[3]))
})

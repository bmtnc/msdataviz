test_that("calculate_price_to_book calculates correctly", {
  expect_equal(calculate_price_to_book(100, 50), 2)
  expect_equal(calculate_price_to_book(75, 25), 3)
})

test_that("calculate_price_to_book handles zero book value", {
  expect_true(is.na(calculate_price_to_book(100, 0)))
})

test_that("calculate_price_to_book handles negative book value", {
  expect_true(is.na(calculate_price_to_book(100, -50)))
})

test_that("calculate_price_to_book is vectorized", {
  result <- calculate_price_to_book(c(100, 75, 100), c(50, 25, 0))
  expect_equal(result[1:2], c(2, 3))
  expect_true(is.na(result[3]))
})

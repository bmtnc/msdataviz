test_that("interest_coverage calculates correctly", {
  expect_equal(interest_coverage(20, 5), 4)
  expect_equal(interest_coverage(30, 10), 3)
})

test_that("interest_coverage handles zero interest", {
  expect_true(is.na(interest_coverage(20, 0)))
})

test_that("interest_coverage handles negative interest", {
  expect_true(is.na(interest_coverage(20, -5)))
})

test_that("interest_coverage is vectorized", {
  result <- interest_coverage(c(20, 30, 20), c(5, 10, 0))
  expect_equal(result[1:2], c(4, 3))
  expect_true(is.na(result[3]))
})

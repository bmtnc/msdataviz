test_that("fcf_conversion calculates correctly", {
  expect_equal(fcf_conversion(80, 100), 0.8)
  expect_equal(fcf_conversion(120, 100), 1.2)
})

test_that("fcf_conversion handles zero nopat", {
  expect_true(is.na(fcf_conversion(80, 0)))
})

test_that("fcf_conversion handles negative nopat", {
  expect_true(is.na(fcf_conversion(80, -100)))
})

test_that("fcf_conversion is vectorized", {
  result <- fcf_conversion(c(80, 120, 80), c(100, 100, 0))
  expect_equal(result[1:2], c(0.8, 1.2))
  expect_true(is.na(result[3]))
})

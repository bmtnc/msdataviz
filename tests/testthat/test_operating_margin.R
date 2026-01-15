test_that("operating_margin calculates correctly", {
  expect_equal(operating_margin(20, 100), 0.2)
  expect_equal(operating_margin(40, 200), 0.2)
})

test_that("operating_margin handles zero revenue", {
  expect_true(is.na(operating_margin(20, 0)))
})

test_that("operating_margin handles negative margins", {
  expect_equal(operating_margin(-10, 100), -0.1)
})

test_that("operating_margin is vectorized", {
  result <- operating_margin(c(20, 40, -10), c(100, 200, 100))
  expect_equal(result, c(0.2, 0.2, -0.1))
})

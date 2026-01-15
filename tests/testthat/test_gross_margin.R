test_that("gross_margin calculates correctly", {
  expect_equal(gross_margin(30, 100), 0.3)
  expect_equal(gross_margin(50, 200), 0.25)
})

test_that("gross_margin handles zero revenue", {
  expect_true(is.na(gross_margin(30, 0)))
})

test_that("gross_margin handles negative margins", {
  expect_equal(gross_margin(-10, 100), -0.1)
})

test_that("gross_margin is vectorized", {
  result <- gross_margin(c(30, 50, -10), c(100, 200, 100))
  expect_equal(result, c(0.3, 0.25, -0.1))
})

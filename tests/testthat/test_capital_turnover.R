test_that("capital_turnover calculates correctly", {
  expect_equal(capital_turnover(200, 100), 2)
  expect_equal(capital_turnover(150, 100), 1.5)
})

test_that("capital_turnover handles zero invested capital", {
  expect_true(is.na(capital_turnover(200, 0)))
})

test_that("capital_turnover handles negative invested capital", {
  expect_true(is.na(capital_turnover(200, -100)))
})

test_that("capital_turnover is vectorized", {
  result <- capital_turnover(c(200, 150, 200), c(100, 100, 0))
  expect_equal(result[1:2], c(2, 1.5))
  expect_true(is.na(result[3]))
})

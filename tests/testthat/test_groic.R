test_that("groic calculates correctly", {
  expect_equal(groic(50, 200), 0.25)
  expect_equal(groic(100, 500), 0.2)
})

test_that("groic handles zero invested capital", {
  expect_true(is.na(groic(50, 0)))
})

test_that("groic handles negative invested capital", {
  expect_true(is.na(groic(50, -100)))
})

test_that("groic is vectorized", {
  result <- groic(c(50, 100, 50), c(200, 500, 0))
  expect_equal(result[1:2], c(0.25, 0.2))
  expect_true(is.na(result[3]))
})

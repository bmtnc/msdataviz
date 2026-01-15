test_that("debt_to_ebitda calculates correctly", {
  expect_equal(debt_to_ebitda(200, 100), 2)
  expect_equal(debt_to_ebitda(150, 50), 3)
})

test_that("debt_to_ebitda handles zero ebitda", {
  expect_true(is.na(debt_to_ebitda(200, 0)))
})

test_that("debt_to_ebitda handles negative ebitda", {
  expect_true(is.na(debt_to_ebitda(200, -100)))
})

test_that("debt_to_ebitda is vectorized", {
  result <- debt_to_ebitda(c(200, 150, 200), c(100, 50, 0))
  expect_equal(result[1:2], c(2, 3))
  expect_true(is.na(result[3]))
})

test_that("cost_of_debt calculates correctly", {
  expect_equal(cost_of_debt(5, 100), 0.05)
  expect_equal(cost_of_debt(10, 200), 0.05)
})

test_that("cost_of_debt handles zero debt", {
  expect_true(is.na(cost_of_debt(5, 0)))
})

test_that("cost_of_debt handles negative debt", {
  expect_true(is.na(cost_of_debt(5, -100)))
})

test_that("cost_of_debt is vectorized", {
  result <- cost_of_debt(c(5, 10, 5), c(100, 200, 0))
  expect_equal(result[1:2], c(0.05, 0.05))
  expect_true(is.na(result[3]))
})

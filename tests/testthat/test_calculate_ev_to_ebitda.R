test_that("calculate_ev_to_ebitda calculates correctly", {
  expect_equal(calculate_ev_to_ebitda(100, 10), 10)
  expect_equal(calculate_ev_to_ebitda(150, 15), 10)
})

test_that("calculate_ev_to_ebitda handles zero ebitda", {
  expect_true(is.na(calculate_ev_to_ebitda(100, 0)))
})

test_that("calculate_ev_to_ebitda handles negative ebitda", {
  expect_true(is.na(calculate_ev_to_ebitda(100, -10)))
})

test_that("calculate_ev_to_ebitda is vectorized", {
  result <- calculate_ev_to_ebitda(c(100, 150, 100), c(10, 15, 0))
  expect_equal(result[1:2], c(10, 10))
  expect_true(is.na(result[3]))
})

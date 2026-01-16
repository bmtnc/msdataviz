test_that("calculate_ev_to_nopat calculates correctly", {
  expect_equal(calculate_ev_to_nopat(100, 5), 20)
  expect_equal(calculate_ev_to_nopat(150, 7.5), 20)
})

test_that("calculate_ev_to_nopat handles zero nopat", {
  expect_true(is.na(calculate_ev_to_nopat(100, 0)))
})

test_that("calculate_ev_to_nopat handles negative nopat", {
  expect_true(is.na(calculate_ev_to_nopat(100, -5)))
})

test_that("calculate_ev_to_nopat is vectorized", {
  result <- calculate_ev_to_nopat(c(100, 150, 100), c(5, 7.5, 0))
  expect_equal(result[1:2], c(20, 20))
  expect_true(is.na(result[3]))
})

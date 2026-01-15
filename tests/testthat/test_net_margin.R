test_that("net_margin calculates correctly", {
  expect_equal(net_margin(10, 100), 0.1)
  expect_equal(net_margin(20, 200), 0.1)
})

test_that("net_margin handles zero revenue", {
  expect_true(is.na(net_margin(10, 0)))
})

test_that("net_margin handles negative margins", {
  expect_equal(net_margin(-5, 100), -0.05)
})

test_that("net_margin is vectorized", {
  result <- net_margin(c(10, 20, -5), c(100, 200, 100))
  expect_equal(result, c(0.1, 0.1, -0.05))
})

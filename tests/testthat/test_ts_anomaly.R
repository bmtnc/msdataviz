test_that("ts_anomaly flags extreme values with direction = 'both'", {
  # Need extreme outliers to exceed 2 std devs
  x <- c(0, 0, 0, 0, 0, 0, 0, 0, -10, 10)
  result <- ts_anomaly(x, threshold = 2, direction = "both")

  expect_type(result, "logical")
  expect_length(result, length(x))
  expect_true(result[9])
  expect_true(result[10])
  expect_false(any(result[1:8]))
})

test_that("ts_anomaly flags only lows with direction = 'low'", {
  x <- c(0, 0, 0, 0, 0, 0, 0, 0, -10, 10)
  result <- ts_anomaly(x, threshold = 2, direction = "low")

  expect_true(result[9])
  expect_false(result[10])
})

test_that("ts_anomaly flags only highs with direction = 'high'", {
  x <- c(0, 0, 0, 0, 0, 0, 0, 0, -10, 10)
  result <- ts_anomaly(x, threshold = 2, direction = "high")

  expect_false(result[9])
  expect_true(result[10])
})

test_that("ts_anomaly defaults to direction = 'both'", {
  x <- c(0, 0, 0, 0, 0, 0, 0, 0, -10, 10)
  result <- ts_anomaly(x, threshold = 2)

  expect_true(result[9])
  expect_true(result[10])
})

test_that("ts_anomaly returns FALSE for empty input", {
  result <- ts_anomaly(numeric(0))
  expect_length(result, 0)
})

test_that("ts_anomaly returns FALSE for single element", {
  result <- ts_anomaly(5)
  expect_equal(result, FALSE)
})

test_that("ts_anomaly returns FALSE for two identical elements", {
  result <- ts_anomaly(c(5, 5))
  expect_equal(result, c(FALSE, FALSE))
})

test_that("ts_anomaly respects custom threshold", {
  x <- c(1, 1, 1, 1, 1, -10)

  result_2 <- ts_anomaly(x, threshold = 2, direction = "low")
  expect_true(result_2[6])

  result_3 <- ts_anomaly(x, threshold = 3, direction = "low")
  expect_false(result_3[6])
})

test_that("ts_anomaly handles NA values", {
  x <- c(1, 1, NA, 1, 1, 1, -50)
  result <- ts_anomaly(x, direction = "low")

  expect_length(result, length(x))
  expect_true(is.na(result[3]))
  expect_true(result[7])
})

test_that("ts_anomaly rejects invalid direction", {
  expect_error(ts_anomaly(c(1, 2, 3), direction = "invalid"))
})

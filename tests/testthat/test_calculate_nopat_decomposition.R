test_that("calculate_nopat_decomposition returns correct structure", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31")),
    ic = c(10000, 11000, 12000, 13000),
    roic = c(0.10, 0.11, 0.10, 0.12)
  )

  result <- calculate_nopat_decomposition(test_data)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("date", "nopat", "nopat_change", "roic_effect", "capital_effect"))
  expect_equal(nrow(result), 4)
})

test_that("calculate_nopat_decomposition calculates nopat correctly", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30")),
    ic = c(10000, 12000, 15000),
    roic = c(0.10, 0.12, 0.08)
  )

  result <- calculate_nopat_decomposition(test_data)

  expected_nopat <- test_data$ic * test_data$roic
  expect_equal(result$nopat, expected_nopat)
})

test_that("calculate_nopat_decomposition components sum to nopat_change", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31")),
    ic = c(10000, 11000, 12000, 13000),
    roic = c(0.10, 0.11, 0.10, 0.12)
  )

  result <- calculate_nopat_decomposition(test_data)

  calculated_change <- result$roic_effect + result$capital_effect
  expect_equal(result$nopat_change, calculated_change)
})

test_that("calculate_nopat_decomposition calculates roic_effect correctly", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30")),
    ic = c(10000, 12000),
    roic = c(0.10, 0.15)
  )

  result <- calculate_nopat_decomposition(test_data)

  base_ic <- 10000
  expected_roic_effect <- base_ic * (test_data$roic - 0.10)
  expect_equal(result$roic_effect, expected_roic_effect)
})

test_that("calculate_nopat_decomposition calculates capital_effect correctly", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30")),
    ic = c(10000, 12000),
    roic = c(0.10, 0.15)
  )

  result <- calculate_nopat_decomposition(test_data)

  expected_capital_effect <- (test_data$ic - 10000) * test_data$roic
  expect_equal(result$capital_effect, expected_capital_effect)
})

test_that("calculate_nopat_decomposition respects base_date parameter", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31")),
    ic = c(10000, 11000, 12000, 13000),
    roic = c(0.10, 0.11, 0.10, 0.12)
  )

  result <- calculate_nopat_decomposition(test_data, base_date = as.Date("2020-06-30"))

  expect_equal(nrow(result), 3)
  expect_equal(min(result$date), as.Date("2020-06-30"))
  expect_equal(result$nopat_change[1], 0)
  expect_equal(result$roic_effect[1], 0)
  expect_equal(result$capital_effect[1], 0)
})

test_that("calculate_nopat_decomposition handles pure capital growth scenario", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30")),
    ic = c(10000, 12000, 15000),
    roic = c(0.10, 0.10, 0.10)
  )

  result <- calculate_nopat_decomposition(test_data)

  expect_equal(result$roic_effect, c(0, 0, 0))
  expect_equal(result$capital_effect, result$nopat_change)
})

test_that("calculate_nopat_decomposition handles pure roic improvement scenario", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30")),
    ic = c(10000, 10000, 10000),
    roic = c(0.10, 0.12, 0.15)
  )

  result <- calculate_nopat_decomposition(test_data)

  expect_equal(result$capital_effect, c(0, 0, 0))
  expect_equal(result$roic_effect, result$nopat_change)
})

test_that("calculate_nopat_decomposition validates required columns", {
  incomplete_data <- data.frame(
    date = as.Date("2020-03-31"),
    ic = 10000
  )

  expect_error(
    calculate_nopat_decomposition(incomplete_data),
    "Required columns missing"
  )
})

test_that("calculate_nopat_decomposition validates non-empty data", {
  empty_data <- data.frame(
    date = as.Date(character()),
    ic = numeric(),
    roic = numeric()
  )

  expect_error(
    calculate_nopat_decomposition(empty_data),
    "must have at least one row"
  )
})

test_that("calculate_nopat_decomposition errors on invalid base_date", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30")),
    ic = c(10000, 12000),
    roic = c(0.10, 0.12)
  )

  expect_error(
    calculate_nopat_decomposition(test_data, base_date = as.Date("2019-01-01")),
    "base_date not found"
  )
})

test_that("plot_tsr_decomposition returns ggplot object", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
    tsr = c(0, 0.2, 0.5),
    market_cap_growth = c(0, 0.1, 0.3),
    share_count_effect = c(0, 0.1, 0.2)
  )

  result <- plot_tsr_decomposition(data, ticker = "TEST")

  expect_s3_class(result, "ggplot")
})

test_that("plot_tsr_decomposition validates required columns", {
  data <- data.frame(
    date = as.Date("2020-01-01"),
    tsr = 0
    # missing other columns
  )

  expect_error(plot_tsr_decomposition(data, ticker = "TEST"))
})

test_that("plot_tsr_decomposition validates ticker", {
  data <- data.frame(
    date = as.Date("2020-01-01"),
    tsr = 0,
    market_cap_growth = 0,
    share_count_effect = 0
  )

  expect_error(plot_tsr_decomposition(data, ticker = ""))
  expect_error(plot_tsr_decomposition(data, ticker = NULL))
})

test_that("plot_tsr_decomposition validates non-empty data", {
  data <- data.frame(
    date = as.Date(character(0)),
    tsr = numeric(0),
    market_cap_growth = numeric(0),
    share_count_effect = numeric(0)
  )

  expect_error(plot_tsr_decomposition(data, ticker = "TEST"))
})

test_that("plot_tsr_decomposition uses provided base_date", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01")),
    tsr = c(0, 0.5),
    market_cap_growth = c(0, 0.3),
    share_count_effect = c(0, 0.2)
  )

  result <- plot_tsr_decomposition(
    data,
    ticker = "TEST",
    base_date = as.Date("2020-01-01")
  )

  expect_s3_class(result, "ggplot")
})

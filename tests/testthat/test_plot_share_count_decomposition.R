test_that("plot_share_count_decomposition returns a ggplot object", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01", "2022-01-01")),
    organic_growth = c(0, 0.10, 0.20),
    share_effect = c(0, 0.05, 0.10),
    per_share_growth = c(0, 0.15, 0.30)
  )

  p <- plot_share_count_decomposition(data, ticker = "AAPL", metric_name = "NOPAT")

  expect_s3_class(p, "ggplot")
})

test_that("plot_share_count_decomposition handles negative share effect (dilution)", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01")),
    organic_growth = c(0, 0.20),
    share_effect = c(0, -0.24),
    per_share_growth = c(0, -0.04)
  )

  p <- plot_share_count_decomposition(data, ticker = "XYZ", metric_name = "Earnings")

  expect_s3_class(p, "ggplot")
})

test_that("plot_share_count_decomposition validates required columns", {
  bad_data <- data.frame(date = as.Date("2020-01-01"), value = 0.1)
  expect_error(plot_share_count_decomposition(bad_data, ticker = "TEST"))
})

test_that("plot_share_count_decomposition validates non-empty data", {
  empty_data <- data.frame(
    date = as.Date(character()),
    organic_growth = numeric(),
    share_effect = numeric(),
    per_share_growth = numeric()
  )
  expect_error(plot_share_count_decomposition(empty_data, ticker = "TEST"))
})

test_that("plot_share_count_decomposition validates ticker parameter", {
  data <- data.frame(
    date = as.Date("2020-01-01"),
    organic_growth = 0,
    share_effect = 0,
    per_share_growth = 0
  )
  expect_error(plot_share_count_decomposition(data, ticker = ""))
})

test_that("plot_share_count_decomposition uses custom metric_name in labels", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2021-01-01")),
    organic_growth = c(0, 0.10),
    share_effect = c(0, 0.05),
    per_share_growth = c(0, 0.15)
  )

  p <- plot_share_count_decomposition(data, ticker = "AAPL", metric_name = "FCF")

  # Check that the metric name appears in the plot labels
  expect_true(grepl("FCF", p$labels$y))
})

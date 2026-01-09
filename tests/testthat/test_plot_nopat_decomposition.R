test_that("plot_nopat_decomposition returns ggplot object", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31")),
    nopat = c(1000, 1210, 1200, 1560) * 1e6,
    nopat_change = c(0, 210, 200, 560) * 1e6,
    roic_effect = c(0, 100, 0, 200) * 1e6,
    capital_effect = c(0, 110, 200, 360) * 1e6
  )

  p <- plot_nopat_decomposition(test_data, ticker = "TEST")

  expect_s3_class(p, "ggplot")
})

test_that("plot_nopat_decomposition validates required columns", {
  incomplete_data <- data.frame(
    date = as.Date("2020-03-31"),
    nopat = 1000
  )

  expect_error(
    plot_nopat_decomposition(incomplete_data, ticker = "TEST"),
    "Required columns missing"
  )
})

test_that("plot_nopat_decomposition validates ticker parameter", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30")),
    nopat = c(1000, 1200),
    nopat_change = c(0, 200),
    roic_effect = c(0, 100),
    capital_effect = c(0, 100)
  )

  expect_error(
    plot_nopat_decomposition(test_data, ticker = ""),
    "ticker"
  )
})

test_that("plot_nopat_decomposition handles base_date parameter", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30")),
    nopat = c(1000, 1200, 1500) * 1e6,
    nopat_change = c(0, 200, 500) * 1e6,
    roic_effect = c(0, 100, 200) * 1e6,
    capital_effect = c(0, 100, 300) * 1e6
  )

  p <- plot_nopat_decomposition(
    test_data,
    ticker = "TEST",
    base_date = as.Date("2020-03-31")
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_nopat_decomposition includes both components in legend", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30")),
    nopat = c(1000, 1200) * 1e6,
    nopat_change = c(0, 200) * 1e6,
    roic_effect = c(0, 100) * 1e6,
    capital_effect = c(0, 100) * 1e6
  )

  p <- plot_nopat_decomposition(test_data, ticker = "TEST")

  built <- ggplot2::ggplot_build(p)
  fill_scale <- built$plot$scales$get_scales("fill")

  expect_true("Capital Deployment" %in% fill_scale$get_labels())
  expect_true("ROIC Effect" %in% fill_scale$get_labels())
})

test_that("plot_nopat_decomposition uses custom metric_name in title", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30")),
    nopat = c(1000, 1200) * 1e6,
    nopat_change = c(0, 200) * 1e6,
    roic_effect = c(0, 100) * 1e6,
    capital_effect = c(0, 100) * 1e6
  )

  p <- plot_nopat_decomposition(
    test_data,
    ticker = "TEST",
    metric_name = "Operating Income"
  )

  expect_true(grepl("Operating Income", p$labels$title))
})

test_that("plot_nopat_decomposition validates non-empty data", {
  empty_data <- data.frame(
    date = as.Date(character()),
    nopat = numeric(),
    nopat_change = numeric(),
    roic_effect = numeric(),
    capital_effect = numeric()
  )

  expect_error(
    plot_nopat_decomposition(empty_data, ticker = "TEST"),
    "must have at least one row"
  )
})

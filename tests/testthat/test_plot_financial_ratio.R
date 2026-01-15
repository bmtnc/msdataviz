test_that("plot_financial_ratio creates a ggplot object", {
  test_data <- data.frame(
    date = as.Date(c("2023-03-31", "2023-06-30", "2023-09-30")),
    gross_margin = c(0.4, 0.42, 0.44)
  )

  p <- plot_financial_ratio(
    data = test_data,
    ratio_cols = "gross_margin",
    labels = "Gross Margin",
    colors = "steelblue",
    y_format = "percent",
    y_label = "Margin"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_financial_ratio handles multiple ratios", {
  test_data <- data.frame(
    date = as.Date(c("2023-03-31", "2023-06-30", "2023-09-30")),
    gross_margin = c(0.4, 0.42, 0.44),
    operating_margin = c(0.2, 0.21, 0.22)
  )

  p <- plot_financial_ratio(
    data = test_data,
    ratio_cols = c("gross_margin", "operating_margin"),
    labels = c("Gross Margin", "Operating Margin"),
    colors = c("steelblue", "darkgreen"),
    y_format = "percent"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_financial_ratio validates matching lengths", {
  test_data <- data.frame(
    date = as.Date("2023-03-31"),
    gross_margin = 0.4
  )

  expect_error(
    plot_financial_ratio(
      data = test_data,
      ratio_cols = c("gross_margin"),
      labels = c("Gross Margin", "Extra"),
      colors = c("steelblue")
    )
  )
})

test_that("plot_financial_ratio validates required columns", {
  test_data <- data.frame(
    date = as.Date("2023-03-31"),
    other_col = 0.4
  )

  expect_error(
    plot_financial_ratio(
      data = test_data,
      ratio_cols = "gross_margin",
      labels = "Gross Margin",
      colors = "steelblue"
    )
  )
})

test_that("plot_financial_ratio handles turns format", {
  test_data <- data.frame(
    date = as.Date(c("2023-03-31", "2023-06-30")),
    turnover = c(1.5, 1.6)
  )

  p <- plot_financial_ratio(
    data = test_data,
    ratio_cols = "turnover",
    labels = "Turnover",
    colors = "steelblue",
    y_format = "turns"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_ic_decomposition returns ggplot object", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31")),
    cum_net_income = c(1000, 2200, 3300, 4600) * 1e6,
    cum_dividends = c(-100, -250, -370, -500) * 1e6,
    debt_change = c(0, 200, 100, 300) * 1e6,
    equity_capital_activity = c(-50, -150, -230, -400) * 1e6,
    ic_change = c(850, 2000, 2800, 4000) * 1e6
  )

  p <- plot_ic_decomposition(test_data, ticker = "TEST")

  expect_s3_class(p, "ggplot")
})

test_that("plot_ic_decomposition validates required columns", {
  incomplete_data <- data.frame(
    date = as.Date("2020-03-31"),
    cum_net_income = 1000
  )

  expect_error(
    plot_ic_decomposition(incomplete_data, ticker = "TEST"),
    "Required columns missing"
  )
})

test_that("plot_ic_decomposition validates ticker parameter", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30")),
    cum_net_income = c(1000, 2000),
    cum_dividends = c(-100, -200),
    debt_change = c(0, 100),
    equity_capital_activity = c(-50, -100),
    ic_change = c(850, 1800)
  )

  expect_error(
    plot_ic_decomposition(test_data, ticker = ""),
    "ticker"
  )
})

test_that("plot_ic_decomposition handles base_date parameter", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30")),
    cum_net_income = c(1000, 2000, 3000),
    cum_dividends = c(-100, -200, -300),
    debt_change = c(0, 100, 50),
    equity_capital_activity = c(-50, -100, -150),
    ic_change = c(850, 1800, 2600)
  )

  p <- plot_ic_decomposition(
    test_data,
    ticker = "TEST",
    base_date = as.Date("2020-03-31")
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_ic_decomposition includes all four components in legend", {
  test_data <- data.frame(
    date = as.Date(c("2020-03-31", "2020-06-30")),
    cum_net_income = c(1000, 2000) * 1e6,
    cum_dividends = c(-100, -200) * 1e6,
    debt_change = c(0, 100) * 1e6,
    equity_capital_activity = c(-50, -100) * 1e6,
    ic_change = c(850, 1800) * 1e6
  )

  p <- plot_ic_decomposition(test_data, ticker = "TEST")

  built <- ggplot2::ggplot_build(p)
  fill_scale <- built$plot$scales$get_scales("fill")

  expect_true("Net Income" %in% fill_scale$get_labels())
  expect_true("Dividends" %in% fill_scale$get_labels())
  expect_true("Debt Change" %in% fill_scale$get_labels())
  expect_true("Equity Capital Activity" %in% fill_scale$get_labels())
})

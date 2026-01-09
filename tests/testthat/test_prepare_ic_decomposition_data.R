test_that("prepare_ic_decomposition_data returns expected list structure", {
  mock_artifacts <- list(
    ttm_data = data.frame(
      ticker = rep("TEST", 4),
      fiscalDateEnding = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31")),
      netIncome.cf = c(1000, 1200, 1100, 1300),
      dividendPayout = c(100, 100, 100, 100),
      shortLongTermDebtTotal = c(5000, 5200, 5100, 5300),
      capitalLeaseObligations = c(0, 0, 0, 0),
      totalShareholderEquity = c(10000, 10500, 10800, 11200)
    )
  )

  result <- prepare_ic_decomposition_data(
    "TEST",
    start_date = as.Date("2020-01-01"),
    artifacts = mock_artifacts
  )

  expect_type(result, "list")
  expect_named(result, c("ic_decomposition_data", "ticker", "base_date"))
  expect_equal(result$ticker, "TEST")
  expect_s3_class(result$base_date, "Date")
})

test_that("prepare_ic_decomposition_data filters by ticker", {
  mock_artifacts <- list(
    ttm_data = data.frame(
      ticker = c("AAPL", "AAPL", "GOOG", "GOOG"),
      fiscalDateEnding = as.Date(c("2020-03-31", "2020-06-30", "2020-03-31", "2020-06-30")),
      netIncome.cf = c(1000, 1200, 2000, 2200),
      dividendPayout = c(100, 100, 0, 0),
      shortLongTermDebtTotal = c(5000, 5200, 1000, 1100),
      capitalLeaseObligations = c(0, 0, 0, 0),
      totalShareholderEquity = c(10000, 10500, 20000, 21000)
    )
  )

  result <- prepare_ic_decomposition_data(
    "AAPL",
    start_date = as.Date("2020-01-01"),
    artifacts = mock_artifacts
  )

  expect_equal(nrow(result$ic_decomposition_data), 2)
})

test_that("prepare_ic_decomposition_data respects start_date filtering", {
  mock_artifacts <- list(
    ttm_data = data.frame(
      ticker = rep("TEST", 4),
      fiscalDateEnding = as.Date(c("2019-12-31", "2020-03-31", "2020-06-30", "2020-09-30")),
      netIncome.cf = c(900, 1000, 1200, 1100),
      dividendPayout = c(100, 100, 100, 100),
      shortLongTermDebtTotal = c(4800, 5000, 5200, 5100),
      capitalLeaseObligations = c(0, 0, 0, 0),
      totalShareholderEquity = c(9500, 10000, 10500, 10800)
    )
  )

  result <- prepare_ic_decomposition_data(
    "TEST",
    start_date = as.Date("2020-01-01"),
    artifacts = mock_artifacts
  )

  expect_equal(nrow(result$ic_decomposition_data), 3)
  expect_true(all(result$ic_decomposition_data$date >= as.Date("2020-01-01")))
})

test_that("prepare_ic_decomposition_data handles insufficient data gracefully", {
  mock_artifacts <- list(
    ttm_data = data.frame(
      ticker = "TEST",
      fiscalDateEnding = as.Date("2020-03-31"),
      netIncome.cf = 1000,
      dividendPayout = 100,
      shortLongTermDebtTotal = 5000,
      capitalLeaseObligations = 0,
      totalShareholderEquity = 10000
    )
  )

  result <- prepare_ic_decomposition_data(
    "TEST",
    start_date = as.Date("2020-01-01"),
    artifacts = mock_artifacts
  )

  expect_null(result$ic_decomposition_data)
  expect_null(result$base_date)
  expect_equal(result$ticker, "TEST")
})

test_that("prepare_ic_decomposition_data validates ticker parameter", {
  mock_artifacts <- list(
    ttm_data = data.frame(
      ticker = "TEST",
      fiscalDateEnding = as.Date("2020-03-31"),
      netIncome.cf = 1000,
      dividendPayout = 100,
      shortLongTermDebtTotal = 5000,
      capitalLeaseObligations = 0,
      totalShareholderEquity = 10000
    )
  )

  expect_error(
    prepare_ic_decomposition_data("", artifacts = mock_artifacts),
    "ticker"
  )
})

test_that("prepare_ic_decomposition_data handles NA values in input", {
  mock_artifacts <- list(
    ttm_data = data.frame(
      ticker = rep("TEST", 3),
      fiscalDateEnding = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30")),
      netIncome.cf = c(1000, NA, 1100),
      dividendPayout = c(100, 100, NA),
      shortLongTermDebtTotal = c(5000, 5200, 5100),
      capitalLeaseObligations = c(NA, 0, 0),
      totalShareholderEquity = c(10000, 10500, 10800)
    )
  )

  result <- prepare_ic_decomposition_data(
    "TEST",
    start_date = as.Date("2020-01-01"),
    artifacts = mock_artifacts
  )

  expect_s3_class(result$ic_decomposition_data, "data.frame")
  expect_equal(nrow(result$ic_decomposition_data), 3)
})

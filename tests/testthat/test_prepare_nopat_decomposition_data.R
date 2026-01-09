test_that("prepare_nopat_decomposition_data returns expected list structure", {
  mock_artifacts <- list(
    ttm_data = data.frame(
      ticker = rep("TEST", 4),
      fiscalDateEnding = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31")),
      ebit_ttm = c(1000, 1100, 1200, 1300),
      depreciationAndAmortization_ttm = c(100, 110, 120, 130),
      depreciation_ttm = c(80, 88, 96, 104),
      totalShareholderEquity = c(5000, 5200, 5400, 5600),
      shortLongTermDebtTotal = c(3000, 3100, 3200, 3300),
      capitalLeaseObligations = c(500, 500, 500, 500)
    )
  )

  result <- prepare_nopat_decomposition_data(
    "TEST",
    start_date = as.Date("2020-01-01"),
    artifacts = mock_artifacts
  )

  expect_type(result, "list")
  expect_named(result, c("nopat_decomposition_data", "ticker", "base_date"))
  expect_equal(result$ticker, "TEST")
  expect_s3_class(result$base_date, "Date")
})

test_that("prepare_nopat_decomposition_data returns correct columns", {
  mock_artifacts <- list(
    ttm_data = data.frame(
      ticker = rep("TEST", 4),
      fiscalDateEnding = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31")),
      ebit_ttm = c(1000, 1100, 1200, 1300),
      depreciationAndAmortization_ttm = c(100, 110, 120, 130),
      depreciation_ttm = c(80, 88, 96, 104),
      totalShareholderEquity = c(5000, 5200, 5400, 5600),
      shortLongTermDebtTotal = c(3000, 3100, 3200, 3300),
      capitalLeaseObligations = c(500, 500, 500, 500)
    )
  )

  result <- prepare_nopat_decomposition_data(
    "TEST",
    start_date = as.Date("2020-01-01"),
    artifacts = mock_artifacts
  )

  expect_named(
    result$nopat_decomposition_data,
    c("date", "nopat", "nopat_change", "roic_effect", "capital_effect")
  )
})

test_that("prepare_nopat_decomposition_data filters by ticker", {
  mock_artifacts <- list(
    ttm_data = data.frame(
      ticker = c("AAPL", "AAPL", "GOOG", "GOOG"),
      fiscalDateEnding = as.Date(c("2020-03-31", "2020-06-30", "2020-03-31", "2020-06-30")),
      ebit_ttm = c(1000, 1100, 2000, 2200),
      depreciationAndAmortization_ttm = c(100, 110, 200, 220),
      depreciation_ttm = c(80, 88, 160, 176),
      totalShareholderEquity = c(5000, 5200, 10000, 10400),
      shortLongTermDebtTotal = c(3000, 3100, 6000, 6200),
      capitalLeaseObligations = c(500, 500, 1000, 1000)
    )
  )

  result <- prepare_nopat_decomposition_data(
    "AAPL",
    start_date = as.Date("2020-01-01"),
    artifacts = mock_artifacts
  )

  expect_equal(nrow(result$nopat_decomposition_data), 2)
})

test_that("prepare_nopat_decomposition_data respects start_date filtering", {
  mock_artifacts <- list(
    ttm_data = data.frame(
      ticker = rep("TEST", 4),
      fiscalDateEnding = as.Date(c("2019-12-31", "2020-03-31", "2020-06-30", "2020-09-30")),
      ebit_ttm = c(900, 1000, 1100, 1200),
      depreciationAndAmortization_ttm = c(90, 100, 110, 120),
      depreciation_ttm = c(72, 80, 88, 96),
      totalShareholderEquity = c(4800, 5000, 5200, 5400),
      shortLongTermDebtTotal = c(2900, 3000, 3100, 3200),
      capitalLeaseObligations = c(500, 500, 500, 500)
    )
  )

  result <- prepare_nopat_decomposition_data(
    "TEST",
    start_date = as.Date("2020-01-01"),
    artifacts = mock_artifacts
  )

  expect_equal(nrow(result$nopat_decomposition_data), 3)
  expect_true(all(result$nopat_decomposition_data$date >= as.Date("2020-01-01")))
})

test_that("prepare_nopat_decomposition_data handles insufficient data gracefully", {
  mock_artifacts <- list(
    ttm_data = data.frame(
      ticker = "TEST",
      fiscalDateEnding = as.Date("2020-03-31"),
      ebit_ttm = 1000,
      depreciationAndAmortization_ttm = 100,
      depreciation_ttm = 80,
      totalShareholderEquity = 5000,
      shortLongTermDebtTotal = 3000,
      capitalLeaseObligations = 500
    )
  )

  result <- prepare_nopat_decomposition_data(
    "TEST",
    start_date = as.Date("2020-01-01"),
    artifacts = mock_artifacts
  )

  expect_null(result$nopat_decomposition_data)
  expect_null(result$base_date)
  expect_equal(result$ticker, "TEST")
})

test_that("prepare_nopat_decomposition_data validates ticker parameter", {
  mock_artifacts <- list(
    ttm_data = data.frame(
      ticker = "TEST",
      fiscalDateEnding = as.Date("2020-03-31"),
      ebit_ttm = 1000,
      depreciationAndAmortization_ttm = 100,
      depreciation_ttm = 80,
      totalShareholderEquity = 5000,
      shortLongTermDebtTotal = 3000,
      capitalLeaseObligations = 500
    )
  )

  expect_error(
    prepare_nopat_decomposition_data("", artifacts = mock_artifacts),
    "ticker"
  )
})

test_that("prepare_nopat_decomposition_data decomposition components sum correctly", {
  mock_artifacts <- list(
    ttm_data = data.frame(
      ticker = rep("TEST", 4),
      fiscalDateEnding = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31")),
      ebit_ttm = c(1000, 1100, 1200, 1300),
      depreciationAndAmortization_ttm = c(100, 110, 120, 130),
      depreciation_ttm = c(80, 88, 96, 104),
      totalShareholderEquity = c(5000, 5200, 5400, 5600),
      shortLongTermDebtTotal = c(3000, 3100, 3200, 3300),
      capitalLeaseObligations = c(500, 500, 500, 500)
    )
  )

  result <- prepare_nopat_decomposition_data(
    "TEST",
    start_date = as.Date("2020-01-01"),
    artifacts = mock_artifacts
  )

  data <- result$nopat_decomposition_data
  expect_equal(
    data$nopat_change,
    data$roic_effect + data$capital_effect,
    tolerance = 1e-10
  )
})

test_that("prepare_nopat_decomposition_data handles end_date filtering", {
  mock_artifacts <- list(
    ttm_data = data.frame(
      ticker = rep("TEST", 4),
      fiscalDateEnding = as.Date(c("2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31")),
      ebit_ttm = c(1000, 1100, 1200, 1300),
      depreciationAndAmortization_ttm = c(100, 110, 120, 130),
      depreciation_ttm = c(80, 88, 96, 104),
      totalShareholderEquity = c(5000, 5200, 5400, 5600),
      shortLongTermDebtTotal = c(3000, 3100, 3200, 3300),
      capitalLeaseObligations = c(500, 500, 500, 500)
    )
  )

  result <- prepare_nopat_decomposition_data(
    "TEST",
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-09-30"),
    artifacts = mock_artifacts
  )

  expect_equal(nrow(result$nopat_decomposition_data), 3)
  expect_true(all(result$nopat_decomposition_data$date <= as.Date("2020-09-30")))
})

test_that("get_report_profile returns default profile with expected fields", {
  result <- get_report_profile("default")

  expect_type(result, "list")
  expect_true("price_decomp_metric" %in% names(result))
  expect_true("price_decomp_numerator" %in% names(result))
  expect_true("kpi_metric" %in% names(result))
  expect_true("dupont_numerator" %in% names(result))
  expect_true("dupont_denominator" %in% names(result))

  expect_equal(result$price_decomp_metric, "nopat")
  expect_equal(result$price_decomp_numerator, "price")
  expect_equal(result$dupont_denominator, "invested_capital")
})

test_that("get_report_profile returns financial profile with ROE settings", {
  result <- get_report_profile("financial")

  expect_equal(result$price_decomp_metric, "bvps")
  expect_equal(result$kpi_metric, "bvps")
  expect_equal(result$dupont_numerator, "netIncome")
  expect_equal(result$dupont_denominator, "equity")
})

test_that("get_report_profile returns asset_heavy profile", {
  result <- get_report_profile("asset_heavy")

  expect_equal(result$price_decomp_metric, "bvps")
  expect_equal(result$dupont_numerator, "nopat")
  expect_equal(result$dupont_denominator, "invested_capital")
})

test_that("get_report_profile returns early_stage profile with gross profit", {
  result <- get_report_profile("early_stage")

  expect_equal(result$price_decomp_metric, "grossProfit")
  expect_equal(result$kpi_metric, "grossProfit")
  expect_equal(result$dupont_numerator, "grossProfit")
})

test_that("get_report_profile uses default when no argument provided", {
  result <- get_report_profile()

  expect_equal(result$price_decomp_metric, "nopat")
})

test_that("get_report_profile errors on unknown profile", {
  expect_error(
    get_report_profile("unknown"),
    "Unknown profile: 'unknown'"
  )
})

test_that("get_report_profile errors on empty string", {
  expect_error(get_report_profile(""))
})

test_that("get_report_profile errors on non-character input", {
  expect_error(get_report_profile(123))
  expect_error(get_report_profile(NULL))
})

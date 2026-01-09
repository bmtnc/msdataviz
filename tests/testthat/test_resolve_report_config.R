test_that("resolve_report_config returns profile defaults when no overrides", {
  result <- resolve_report_config(profile = "default")

  expect_equal(result$price_decomp_metric, "nopat")
  expect_equal(result$price_decomp_numerator, "price")
  expect_equal(result$kpi_metric, "nopat")
  expect_equal(result$dupont_numerator, "nopat")
  expect_equal(result$dupont_denominator, "invested_capital")
})

test_that("resolve_report_config overrides price_decomp_metric", {
  result <- resolve_report_config(
    profile = "default",
    price_decomp_metric = "bvps"
  )

  expect_equal(result$price_decomp_metric, "bvps")
  expect_equal(result$dupont_numerator, "nopat")
})

test_that("resolve_report_config overrides price_decomp_numerator", {
  result <- resolve_report_config(
    profile = "default",
    price_decomp_numerator = "ev"
  )

  expect_equal(result$price_decomp_numerator, "ev")
  expect_equal(result$price_decomp_metric, "nopat")
})

test_that("resolve_report_config overrides kpi_metric", {
  result <- resolve_report_config(
    profile = "default",
    kpi_metric = "grossProfit"
  )

  expect_equal(result$kpi_metric, "grossProfit")
  expect_equal(result$price_decomp_metric, "nopat")
})

test_that("resolve_report_config overrides dupont_numerator", {
  result <- resolve_report_config(
    profile = "default",
    dupont_numerator = "netIncome"
  )

  expect_equal(result$dupont_numerator, "netIncome")
  expect_equal(result$dupont_denominator, "invested_capital")
})

test_that("resolve_report_config overrides dupont_denominator", {
  result <- resolve_report_config(
    profile = "default",
    dupont_denominator = "equity"
  )

  expect_equal(result$dupont_denominator, "equity")
  expect_equal(result$dupont_numerator, "nopat")
})

test_that("resolve_report_config applies multiple overrides", {
  result <- resolve_report_config(
    profile = "default",
    price_decomp_metric = "grossProfit",
    dupont_numerator = "netIncome",
    dupont_denominator = "equity"
  )

  expect_equal(result$price_decomp_metric, "grossProfit")
  expect_equal(result$dupont_numerator, "netIncome")
  expect_equal(result$dupont_denominator, "equity")
  expect_equal(result$price_decomp_numerator, "price")
})

test_that("resolve_report_config works with non-default profile", {
  result <- resolve_report_config(profile = "financial")

  expect_equal(result$price_decomp_metric, "bvps")
  expect_equal(result$dupont_denominator, "equity")
})

test_that("resolve_report_config overrides financial profile", {
  result <- resolve_report_config(
    profile = "financial",
    dupont_denominator = "invested_capital"
  )

  expect_equal(result$price_decomp_metric, "bvps")
  expect_equal(result$dupont_denominator, "invested_capital")
})

test_that("resolve_report_config uses default profile when no argument", {
  result <- resolve_report_config()

  expect_equal(result$price_decomp_metric, "nopat")
})

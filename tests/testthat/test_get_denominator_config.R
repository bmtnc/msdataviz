test_that("get_denominator_config returns equity config with expected fields", {
  result <- get_denominator_config("equity")

  expect_type(result, "list")
  expect_true("display_name" %in% names(result))
  expect_true("columns" %in% names(result))
  expect_true("is_calculated" %in% names(result))
  expect_true("return_label" %in% names(result))
  expect_true("effect_label" %in% names(result))
  expect_true("multiplier_label" %in% names(result))
  expect_true("title_suffix" %in% names(result))

  expect_equal(result$display_name, "Equity")
  expect_equal(result$return_label, "ROE")
  expect_equal(result$effect_label, "Financial Leverage Effect")
  expect_false(result$is_calculated)
})

test_that("get_denominator_config returns invested_capital config", {
  result <- get_denominator_config("invested_capital")

  expect_equal(result$display_name, "Invested Capital")
  expect_equal(result$return_label, "ROIC")
  expect_equal(result$effect_label, "Capital Efficiency Effect")
  expect_equal(result$multiplier_label, "Assets / IC")
  expect_true(result$is_calculated)
  expect_true(length(result$columns) > 1)
})

test_that("get_denominator_config returns total_assets config", {
  result <- get_denominator_config("total_assets")

  expect_equal(result$display_name, "Total Assets")
  expect_equal(result$return_label, "ROA")
  expect_equal(result$effect_label, "Asset Turnover Effect")
  expect_false(result$is_calculated)
})

test_that("get_denominator_config errors on unknown denominator", {
  expect_error(
    get_denominator_config("unknown"),
    "Unknown denominator: 'unknown'"
  )
})

test_that("get_denominator_config errors on empty string", {
  expect_error(get_denominator_config(""))
})

test_that("get_denominator_config errors on non-character input", {
  expect_error(get_denominator_config(123))
  expect_error(get_denominator_config(NULL))
})

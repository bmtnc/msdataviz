test_that("get_dupont_labels returns standard ROIC labels for nopat + invested_capital", {
  result <- get_dupont_labels("nopat", "invested_capital")

  expect_equal(result$return_label, "ROIC")
  expect_equal(result$roa_label, "ROA")
  expect_equal(result$title_suffix, "ROIC Decomposition")
  expect_null(result$footnote)
})

test_that("get_dupont_labels returns standard ROE labels for netIncome + equity", {
  result <- get_dupont_labels("netIncome", "equity")

  expect_equal(result$return_label, "ROE")
  expect_equal(result$roa_label, "ROA")
  expect_equal(result$title_suffix, "ROE Decomposition (DuPont)")
  expect_null(result$footnote)
})

test_that("get_dupont_labels returns GROIC and GROA for grossProfit + invested_capital", {
  result <- get_dupont_labels("grossProfit", "invested_capital")

  expect_equal(result$return_label, "GROIC")
  expect_equal(result$roa_label, "GROA")
  expect_equal(result$title_suffix, "GROIC Decomposition")
  expect_true(!is.null(result$footnote))
  expect_true(grepl("upper bound", result$footnote))
})

test_that("get_dupont_labels returns GROE and GROA for grossProfit + equity", {
  result <- get_dupont_labels("grossProfit", "equity")

  expect_equal(result$return_label, "GROE")
  expect_equal(result$roa_label, "GROA")
  expect_equal(result$title_suffix, "GROE Decomposition")
  expect_true(!is.null(result$footnote))
})

test_that("get_dupont_labels returns GROA for grossProfit + total_assets", {
  result <- get_dupont_labels("grossProfit", "total_assets")

  expect_equal(result$return_label, "GROA")
  expect_equal(result$roa_label, "GROA")
  expect_equal(result$title_suffix, "GROA Decomposition")
  expect_true(!is.null(result$footnote))
})

test_that("get_dupont_labels validates inputs", {
  expect_error(get_dupont_labels("", "equity"))
  expect_error(get_dupont_labels("nopat", ""))
  expect_error(get_dupont_labels(NULL, "equity"))
})

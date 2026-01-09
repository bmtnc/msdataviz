test_that("render_decomposition_report validates ticker", {
  expect_error(render_decomposition_report(""))
  expect_error(render_decomposition_report(NULL))
  expect_error(render_decomposition_report(123))
})

test_that("render_decomposition_report validates profile", {
  # Bad profile should fail when resolve_report_config is called
  expect_error(
    render_decomposition_report("AAPL", profile = "invalid_profile"),
    "Unknown profile"
  )
})

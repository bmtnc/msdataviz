test_that("cross_sectional_anomaly detects low outliers", {
  # Distribution with mean=5.5, sd≈3.03
  distribution <- 1:10

  # Value within 2 sd
  expect_false(cross_sectional_anomaly(0, distribution, direction = "low"))

  # Value below 2 sd (5.5 - 2*3.03 ≈ -0.56)
  expect_true(cross_sectional_anomaly(-2, distribution, direction = "low"))
})

test_that("cross_sectional_anomaly detects high outliers", {
  # Distribution with mean=5.5, sd≈3.03
  distribution <- 1:10

  # Value within 2 sd
  expect_false(cross_sectional_anomaly(11, distribution, direction = "high"))

  # Value above 2 sd (5.5 + 2*3.03 ≈ 11.56)
  expect_true(cross_sectional_anomaly(13, distribution, direction = "high"))
})

test_that("cross_sectional_anomaly detects both directions", {
  distribution <- 1:10

  # Low outlier
  expect_true(cross_sectional_anomaly(-2, distribution, direction = "both"))

  # High outlier
  expect_true(cross_sectional_anomaly(13, distribution, direction = "both"))

  # Within bounds
  expect_false(cross_sectional_anomaly(5, distribution, direction = "both"))
})

test_that("cross_sectional_anomaly defaults to direction = 'both'", {
  distribution <- 1:10

  expect_true(cross_sectional_anomaly(-2, distribution))
  expect_true(cross_sectional_anomaly(13, distribution))
  expect_false(cross_sectional_anomaly(5, distribution))
})

test_that("cross_sectional_anomaly respects custom threshold", {
  distribution <- 1:10
  # mean=5.5, sd≈3.03
  # With threshold=1: bounds ≈ 2.47 to 8.53
  # With threshold=3: bounds ≈ -3.59 to 14.59

  # 1 is outlier with threshold=1, not with threshold=3
  expect_true(cross_sectional_anomaly(1, distribution, threshold = 1, direction = "low"))
  expect_false(cross_sectional_anomaly(1, distribution, threshold = 3, direction = "low"))
})

test_that("cross_sectional_anomaly returns FALSE for small distributions", {
  expect_false(cross_sectional_anomaly(100, numeric(0)))
  expect_false(cross_sectional_anomaly(100, c(5)))
})

test_that("cross_sectional_anomaly returns FALSE when sd is zero", {
  # All same values = sd of 0
  distribution <- rep(5, 10)
  expect_false(cross_sectional_anomaly(100, distribution))
  expect_false(cross_sectional_anomaly(-100, distribution))
})

test_that("cross_sectional_anomaly handles NA in distribution", {
  distribution <- c(1:10, NA, NA)

  # Should still work, ignoring NAs
  expect_true(cross_sectional_anomaly(-2, distribution, direction = "low"))
  expect_false(cross_sectional_anomaly(5, distribution, direction = "low"))
})

test_that("cross_sectional_anomaly rejects invalid direction", {
  expect_error(cross_sectional_anomaly(5, 1:10, direction = "invalid"))
})

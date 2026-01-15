test_that("prepare_peer_drawdowns returns correct structure", {
  price_data <- data.frame(
    ticker = rep(c("AAPL", "MSFT", "GOOG"), each = 5),
    date = rep(as.Date("2024-01-01") + 0:4, 3),
    adjusted_close = c(
      100, 105, 103, 110, 108,
      200, 210, 205, 220, 215,
      300, 290, 295, 310, 305
    )
  )

  ttm_data <- data.frame(
    ticker = c("AAPL", "MSFT", "GOOG"),
    sector = rep("Technology", 3)
  )

  result <- prepare_peer_drawdowns(
    ticker = "AAPL",
    start_date = as.Date("2024-01-01"),
    peer_group = "sector",
    price_data = price_data,
    ttm_data = ttm_data
  )

  expect_s3_class(result, "data.frame")
  expect_true(all(c("date", "ticker", "drawdown") %in% names(result)))
  # Target ticker should be EXCLUDED from peers
  expect_true(all(result$ticker %in% c("MSFT", "GOOG")))
  expect_false("AAPL" %in% result$ticker)
})

test_that("prepare_peer_drawdowns calculates drawdowns correctly", {
  # Use MSFT as target, AAPL as peer
  price_data <- data.frame(
    ticker = rep(c("AAPL", "MSFT"), each = 5),
    date = rep(as.Date("2024-01-01") + 0:4, 2),
    adjusted_close = c(
      100, 110, 105, 120, 115,  # AAPL (peer)
      200, 210, 205, 220, 215   # MSFT (target, excluded)
    )
  )

  ttm_data <- data.frame(
    ticker = c("AAPL", "MSFT"),
    sector = rep("Technology", 2)
  )

  result <- prepare_peer_drawdowns(
    ticker = "MSFT",
    start_date = as.Date("2024-01-01"),
    peer_group = "sector",
    price_data = price_data,
    ttm_data = ttm_data
  )

  # Only AAPL should be in results (MSFT is target)
  expect_equal(unique(result$ticker), "AAPL")

  # AAPL drawdowns: high=100, 110, 110, 120, 120
  expected_drawdowns <- c(
    0,
    0,
    (105 - 110) / 110,
    0,
    (115 - 120) / 120
  )

  expect_equal(result$drawdown, expected_drawdowns, tolerance = 1e-10)
})

test_that("prepare_peer_drawdowns respects date filters", {
  price_data <- data.frame(
    ticker = rep(c("AAPL", "MSFT"), each = 10),
    date = rep(as.Date("2024-01-01") + 0:9, 2),
    adjusted_close = c(100:109, 200:209)
  )

  ttm_data <- data.frame(
    ticker = c("AAPL", "MSFT"),
    sector = rep("Technology", 2)
  )

  result <- prepare_peer_drawdowns(
    ticker = "AAPL",
    start_date = as.Date("2024-01-03"),
    end_date = as.Date("2024-01-07"),
    peer_group = "sector",
    price_data = price_data,
    ttm_data = ttm_data
  )

  # Only MSFT should be in results (AAPL is target)
  expect_equal(unique(result$ticker), "MSFT")
  expect_equal(nrow(result), 5)
  expect_equal(min(result$date), as.Date("2024-01-03"))
  expect_equal(max(result$date), as.Date("2024-01-07"))
})

test_that("prepare_peer_drawdowns excludes NA prices", {
  price_data <- data.frame(
    ticker = rep(c("AAPL", "MSFT"), each = 5),
    date = rep(as.Date("2024-01-01") + 0:4, 2),
    adjusted_close = c(
      100, NA, 105, 110, NA,  # AAPL (peer) - 3 valid rows
      200, 210, NA, 220, 215  # MSFT (target, excluded)
    )
  )

  ttm_data <- data.frame(
    ticker = c("AAPL", "MSFT"),
    sector = rep("Technology", 2)
  )

  result <- prepare_peer_drawdowns(
    ticker = "MSFT",
    start_date = as.Date("2024-01-01"),
    peer_group = "sector",
    price_data = price_data,
    ttm_data = ttm_data
  )

  expect_equal(nrow(result), 3)
  expect_false(any(is.na(result$drawdown)))
})

test_that("prepare_peer_drawdowns validates inputs", {
  price_data <- data.frame(
    ticker = "AAPL",
    date = as.Date("2024-01-01"),
    adjusted_close = 100
  )

  ttm_data <- data.frame(
    ticker = "AAPL",
    sector = "Technology"
  )

  expect_error(
    prepare_peer_drawdowns(
      ticker = "",
      start_date = as.Date("2024-01-01"),
      peer_group = "sector",
      price_data = price_data,
      ttm_data = ttm_data
    )
  )
})

test_that("prepare_peer_drawdowns filters by peer_group correctly", {
  price_data <- data.frame(
    ticker = rep(c("AAPL", "MSFT", "GOOG", "META"), each = 3),
    date = rep(as.Date("2024-01-01") + 0:2, 4),
    adjusted_close = rep(100, 12)
  )

  ttm_data <- data.frame(
    ticker = c("AAPL", "MSFT", "GOOG", "META"),
    sector = c("Technology", "Technology", "Technology", "Technology"),
    subsector = c("Hardware", "Software", "Software", "Software"),
    industry = c("Consumer Electronics", "Enterprise", "Search", "Social")
  )

  # Sector: 3 peers (MSFT, GOOG, META) - AAPL is excluded as target
  result_sector <- prepare_peer_drawdowns(
    ticker = "AAPL",
    start_date = as.Date("2024-01-01"),
    peer_group = "sector",
    price_data = price_data,
    ttm_data = ttm_data
  )
  expect_equal(length(unique(result_sector$ticker)), 3)
  expect_false("AAPL" %in% result_sector$ticker)

  # Subsector Software: 2 peers (GOOG, META) when MSFT is target
  result_subsector <- prepare_peer_drawdowns(
    ticker = "MSFT",
    start_date = as.Date("2024-01-01"),
    peer_group = "subsector",
    price_data = price_data,
    ttm_data = ttm_data
  )
  expect_equal(sort(unique(result_subsector$ticker)), c("GOOG", "META"))
  expect_false("MSFT" %in% result_subsector$ticker)

  # Industry Search: 0 peers when GOOG is target (only GOOG in Search)
  result_industry <- prepare_peer_drawdowns(
    ticker = "GOOG",
    start_date = as.Date("2024-01-01"),
    peer_group = "industry",
    price_data = price_data,
    ttm_data = ttm_data
  )
  expect_equal(nrow(result_industry), 0)
})

test_that("prepare_peer_drawdowns defaults to sector", {
  price_data <- data.frame(
    ticker = rep(c("AAPL", "MSFT", "GOOG"), each = 3),
    date = rep(as.Date("2024-01-01") + 0:2, 3),
    adjusted_close = rep(100, 9)
  )

  ttm_data <- data.frame(
    ticker = c("AAPL", "MSFT", "GOOG"),
    sector = c("Technology", "Technology", "Technology")
  )

  result <- prepare_peer_drawdowns(
    ticker = "AAPL",
    start_date = as.Date("2024-01-01"),
    price_data = price_data,
    ttm_data = ttm_data
  )

  # Should have 2 peers (MSFT, GOOG) - AAPL is excluded
  expect_equal(length(unique(result$ticker)), 2)
  expect_false("AAPL" %in% result$ticker)
})

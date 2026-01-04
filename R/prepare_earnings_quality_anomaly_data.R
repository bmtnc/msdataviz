#' Prepare Earnings Quality Anomaly Data
#'
#' Prepares cross-sectional data for earnings quality anomaly detection.
#' Compares P/E (reported earnings) vs P/FFO ex SBC (cash earnings adjusted for SBC).
#' SBC is derived as: OCF - Net Income - D&A
#' FFO ex SBC = Net Income + D&A (treating SBC as a real expense)
#'
#' @param ticker Target ticker symbol
#' @param artifacts Optional pre-loaded artifacts list
#' @param cache_dir Directory for cache files
#' @param max_cache_age_days Maximum cache age before refresh
#' @param s3_bucket S3 bucket name
#' @param aws_region AWS region
#' @return List with: data (data frame), ticker, sector_name, subsector_name
#' @export
prepare_earnings_quality_anomaly_data <- function(
    ticker,
    artifacts = NULL,
    cache_dir = "~/.cache/msdataviz",
    max_cache_age_days = 1,
    s3_bucket = Sys.getenv("S3_BUCKET", "avpipeline-artifacts-prod"),
    aws_region = Sys.getenv("AWS_REGION", "us-east-1")
) {
  avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")

  if (is.null(artifacts)) {
    artifacts <- get_cached_artifacts(
      cache_dir = cache_dir,
      max_age_days = max_cache_age_days,
      s3_bucket = s3_bucket,
      aws_region = aws_region
    )
  }

  price_data <- artifacts$price_data
  ttm_data <- artifacts$ttm_data

  # Get sector, subsector for target ticker
  sector_name <- get_ticker_sector(ticker, ttm_data)
  subsector_name <- get_ticker_subsector(ticker, ttm_data)
  sector_tickers <- get_sector_tickers(sector_name, ttm_data)
  subsector_tickers <- get_subsector_tickers(subsector_name, ttm_data)

  # Get latest prices
  latest_date <- max(price_data$date)

  latest_prices <- price_data %>%
    dplyr::filter(ticker %in% sector_tickers) %>%
    dplyr::filter(!is.na(adjusted_close)) %>%
    dplyr::group_by(ticker) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(ticker, price = adjusted_close)

  # Get latest TTM fundamentals
  latest_ttm <- ttm_data %>%
    dplyr::filter(ticker %in% sector_tickers) %>%
    dplyr::group_by(ticker) %>%
    dplyr::filter(fiscalDateEnding == max(fiscalDateEnding)) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      ticker,
      subsector,
      industry,
      net_income = netIncome_ttm,
      ocf = operatingCashflow_ttm,
      da = depreciationAndAmortization_ttm,
      shares = commonStockSharesOutstanding
    )

  # Calculate metrics
  anomaly_data <- latest_prices %>%
    dplyr::inner_join(latest_ttm, by = "ticker") %>%
    dplyr::mutate(
      # EPS = Net Income / Shares
      eps = net_income / shares,
      # FFO ex SBC = Net Income + D&A (SBC treated as real expense)
      # This is because: SBC ≈ OCF - Net Income - D&A
      # So: FFO ex SBC = OCF - SBC = Net Income + D&A
      ffo_ex_sbc_per_share = (net_income + da) / shares,
      # P/E ratio
      pe_ratio = price / eps,
      # P/FFO ex SBC ratio
      p_ffo_ex_sbc = price / ffo_ex_sbc_per_share
    ) %>%
    # Filter for valid, positive ratios (exclude negative earnings)
    dplyr::filter(
      !is.na(pe_ratio) & is.finite(pe_ratio) & pe_ratio > 0 &
      !is.na(p_ffo_ex_sbc) & is.finite(p_ffo_ex_sbc) & p_ffo_ex_sbc > 0 &
      pe_ratio < 200 & p_ffo_ex_sbc < 200  # Cap extreme values
    ) %>%
    dplyr::select(ticker, subsector, industry, pe_ratio, p_ffo_ex_sbc)

  list(
    data = anomaly_data,
    ticker = ticker,
    sector_name = sector_name,
    subsector_name = subsector_name,
    n_sector_stocks = length(sector_tickers),
    n_subsector_stocks = length(subsector_tickers)
  )
}

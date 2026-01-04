#' Prepare DuPont Anomaly Data
#'
#' Prepares cross-sectional data for DuPont-based anomaly detection.
#' Calculates operating margin and asset turnover for all tickers in sector.
#'
#' @param ticker Target ticker symbol
#' @param artifacts Optional pre-loaded artifacts list
#' @param cache_dir Directory for cache files
#' @param max_cache_age_days Maximum cache age before refresh
#' @param s3_bucket S3 bucket name
#' @param aws_region AWS region
#' @return List with: data (data frame), ticker, sector_name, subsector_name, industry_name
#' @export
prepare_dupont_anomaly_data <- function(
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

  ttm_data <- artifacts$ttm_data

  # Get sector, subsector, and industry for target ticker
  sector_name <- get_ticker_sector(ticker, ttm_data)
  subsector_name <- get_ticker_subsector(ticker, ttm_data)
  industry_name <- get_ticker_industry(ticker, ttm_data)
  sector_tickers <- get_sector_tickers(sector_name, ttm_data)
  subsector_tickers <- get_subsector_tickers(subsector_name, ttm_data)

  # Get latest TTM data for each ticker in sector
  latest_ttm <- ttm_data %>%
    dplyr::filter(ticker %in% sector_tickers) %>%
    dplyr::group_by(ticker) %>%
    dplyr::filter(fiscalDateEnding == max(fiscalDateEnding)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()

  # Calculate operating margin and asset turnover
  anomaly_data <- latest_ttm %>%
    dplyr::filter(
      !is.na(operatingIncome_ttm) &
      !is.na(totalRevenue_ttm) &
      !is.na(totalAssets) &
      totalRevenue_ttm > 0 &
      totalAssets > 0
    ) %>%
    dplyr::mutate(
      operating_margin = (operatingIncome_ttm / totalRevenue_ttm) * 100,
      asset_turnover = totalRevenue_ttm / totalAssets
    ) %>%
    dplyr::filter(
      is.finite(operating_margin) &
      is.finite(asset_turnover) &
      operating_margin >= 0
    ) %>%
    dplyr::select(ticker, subsector, industry, operating_margin, asset_turnover)

  list(
    data = anomaly_data,
    ticker = ticker,
    sector_name = sector_name,
    subsector_name = subsector_name,
    industry_name = industry_name,
    n_sector_stocks = length(sector_tickers),
    n_subsector_stocks = length(subsector_tickers)
  )
}

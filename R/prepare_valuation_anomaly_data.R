#' Prepare Valuation Anomaly Data
#'
#' Prepares cross-sectional data for valuation anomaly detection.
#' Calculates current EV/NOPAT and YoY change for all tickers in sector.
#'
#' @param ticker Target ticker symbol
#' @param artifacts Optional pre-loaded artifacts list
#' @param cache_dir Directory for cache files
#' @param max_cache_age_days Maximum cache age before refresh
#' @param s3_bucket S3 bucket name
#' @param aws_region AWS region
#' @return List with: data (data frame), ticker, sector_name, industry_name
#' @export
prepare_valuation_anomaly_data <- function(
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

  # Get sector and industry for target ticker
  sector_name <- get_ticker_sector(ticker, ttm_data)
  industry_name <- get_ticker_industry(ticker, ttm_data)
  sector_tickers <- get_sector_tickers(sector_name, ttm_data)

  # Get latest price for each ticker
  latest_prices <- price_data %>%
    dplyr::filter(ticker %in% sector_tickers) %>%
    dplyr::group_by(ticker) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(ticker, date, price = adjusted_close)

  # Get latest and year-ago TTM data
  latest_ttm <- ttm_data %>%
    dplyr::filter(ticker %in% sector_tickers) %>%
    dplyr::group_by(ticker) %>%
    dplyr::filter(fiscalDateEnding == max(fiscalDateEnding)) %>%
    dplyr::ungroup()

  # Calculate per-share metrics for EV/NOPAT
  latest_ttm <- latest_ttm %>%
    dplyr::mutate(
      ebit_ttm_per_share = ebit_ttm / commonStockSharesOutstanding,
      dep_amort_ttm_per_share = depreciationAndAmortization_ttm / commonStockSharesOutstanding,
      depreciation_ttm_per_share = depreciation_ttm / commonStockSharesOutstanding,
      debt_total_per_share = shortLongTermDebtTotal / commonStockSharesOutstanding,
      lease_obligations_per_share = capitalLeaseObligations / commonStockSharesOutstanding,
      cash_st_investments_per_share = cashAndShortTermInvestments / commonStockSharesOutstanding,
      lt_investments_per_share = longTermInvestments / commonStockSharesOutstanding,
      nopat_per_share = avpipeline:::calculate_nopat_per_share(
        ebit_ttm_per_share,
        dep_amort_ttm_per_share,
        depreciation_ttm_per_share
      )
    ) %>%
    dplyr::select(
      ticker, sector, industry, fiscalDateEnding,
      nopat_per_share, debt_total_per_share, lease_obligations_per_share,
      cash_st_investments_per_share, lt_investments_per_share
    )

  # Join price to TTM and calculate EV/NOPAT
  current_data <- latest_prices %>%
    dplyr::inner_join(latest_ttm, by = "ticker") %>%
    dplyr::mutate(
      ev_per_share = avpipeline:::calculate_enterprise_value_per_share(
        price,
        debt_total_per_share,
        lease_obligations_per_share,
        cash_st_investments_per_share,
        lt_investments_per_share
      ),
      ev_nopat = ev_per_share / nopat_per_share
    ) %>%
    dplyr::filter(!is.na(ev_nopat) & is.finite(ev_nopat) & ev_nopat > 0)

  # Calculate YoY change in EV/NOPAT
  # Get price from 1 year ago
  one_year_ago <- max(latest_prices$date) - 365
  year_ago_prices <- price_data %>%
    dplyr::filter(ticker %in% sector_tickers) %>%
    dplyr::filter(date <= one_year_ago) %>%
    dplyr::group_by(ticker) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(ticker, price_1y = adjusted_close)

  # Get TTM data from ~1 year ago
  year_ago_ttm <- ttm_data %>%
    dplyr::filter(ticker %in% sector_tickers) %>%
    dplyr::filter(fiscalDateEnding <= one_year_ago) %>%
    dplyr::group_by(ticker) %>%
    dplyr::filter(fiscalDateEnding == max(fiscalDateEnding)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      ebit_ttm_per_share = ebit_ttm / commonStockSharesOutstanding,
      dep_amort_ttm_per_share = depreciationAndAmortization_ttm / commonStockSharesOutstanding,
      depreciation_ttm_per_share = depreciation_ttm / commonStockSharesOutstanding,
      debt_total_per_share = shortLongTermDebtTotal / commonStockSharesOutstanding,
      lease_obligations_per_share = capitalLeaseObligations / commonStockSharesOutstanding,
      cash_st_investments_per_share = cashAndShortTermInvestments / commonStockSharesOutstanding,
      lt_investments_per_share = longTermInvestments / commonStockSharesOutstanding,
      nopat_per_share_1y = avpipeline:::calculate_nopat_per_share(
        ebit_ttm_per_share,
        dep_amort_ttm_per_share,
        depreciation_ttm_per_share
      )
    ) %>%
    dplyr::select(
      ticker, nopat_per_share_1y, debt_total_per_share_1y = debt_total_per_share,
      lease_obligations_per_share_1y = lease_obligations_per_share,
      cash_st_investments_per_share_1y = cash_st_investments_per_share,
      lt_investments_per_share_1y = lt_investments_per_share
    )

  # Calculate year-ago EV/NOPAT
  year_ago_data <- year_ago_prices %>%
    dplyr::inner_join(year_ago_ttm, by = "ticker") %>%
    dplyr::mutate(
      ev_per_share_1y = avpipeline:::calculate_enterprise_value_per_share(
        price_1y,
        debt_total_per_share_1y,
        lease_obligations_per_share_1y,
        cash_st_investments_per_share_1y,
        lt_investments_per_share_1y
      ),
      ev_nopat_1y = ev_per_share_1y / nopat_per_share_1y
    ) %>%
    dplyr::filter(!is.na(ev_nopat_1y) & is.finite(ev_nopat_1y) & ev_nopat_1y > 0) %>%
    dplyr::select(ticker, ev_nopat_1y)

  # Join current and year-ago, calculate YoY change
  # Clip EV/NOPAT at 100 to handle extreme outliers
 anomaly_data <- current_data %>%
    dplyr::inner_join(year_ago_data, by = "ticker") %>%
    dplyr::mutate(
      ev_nopat_raw = ev_nopat,
      ev_nopat = pmin(ev_nopat, 100),
      ev_nopat_yoy_change = (ev_nopat_raw / ev_nopat_1y) - 1
    ) %>%
    dplyr::select(ticker, sector, industry, ev_nopat, ev_nopat_yoy_change)

  list(
    data = anomaly_data,
    ticker = ticker,
    sector_name = sector_name,
    industry_name = industry_name
  )
}

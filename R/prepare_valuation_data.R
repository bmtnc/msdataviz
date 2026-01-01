#' Prepare EV/NOPAT Valuation Data
#'
#' Prepares daily EV/NOPAT ratio data for a ticker with sector and industry medians.
#' Uses forward-filled quarterly fundamentals joined to daily prices.
#'
#' @param ticker Character string for the ticker symbol
#' @param start_date Start date for filtering (default: 2017-12-31)
#' @param end_date End date for filtering (default: NULL)
#' @param artifacts Optional pre-loaded artifacts list
#' @param cache_dir Directory for cache files
#' @param max_cache_age_days Maximum cache age before refresh
#' @param s3_bucket S3 bucket name
#' @param aws_region AWS region
#' @return List with: valuation_data, ticker, sector_name, industry_name, n_sector_stocks, n_industry_stocks, metric_name
#' @export
prepare_valuation_data <- function(
    ticker,
    start_date = as.Date("2017-12-31"),
    end_date = NULL,
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

  # Get sector and industry info
  sector_name <- get_ticker_sector(ticker, ttm_data)
  industry_name <- get_ticker_industry(ticker, ttm_data)
  sector_tickers <- get_sector_tickers(sector_name, ttm_data)
  industry_tickers <- get_industry_tickers(industry_name, ttm_data)

  # Select columns needed for EV/NOPAT calculation
  ttm_subset <- ttm_data %>%
    dplyr::select(
      ticker, fiscalDateEnding, sector,
      commonStockSharesOutstanding,
      # NOPAT columns
      ebit_ttm, depreciationAndAmortization_ttm, depreciation_ttm,
      # EV columns (balance sheet - not TTM)
      shortLongTermDebtTotal, capitalLeaseObligations,
      cashAndShortTermInvestments, longTermInvestments
    )

  # Calculate per-share columns for EV/NOPAT
  ttm_subset <- ttm_subset %>%
    dplyr::mutate(
      ebit_ttm_per_share = ebit_ttm / commonStockSharesOutstanding,
      dep_amort_ttm_per_share = depreciationAndAmortization_ttm / commonStockSharesOutstanding,
      depreciation_ttm_per_share = depreciation_ttm / commonStockSharesOutstanding,
      debt_total_per_share = shortLongTermDebtTotal / commonStockSharesOutstanding,
      lease_obligations_per_share = capitalLeaseObligations / commonStockSharesOutstanding,
      cash_st_investments_per_share = cashAndShortTermInvestments / commonStockSharesOutstanding,
      lt_investments_per_share = longTermInvestments / commonStockSharesOutstanding,
      # Calculate NOPAT per share using avpipeline logic
      nopat_per_share = avpipeline:::calculate_nopat_per_share(
        ebit_ttm_per_share,
        dep_amort_ttm_per_share,
        depreciation_ttm_per_share
      )
    )

  filtered_prices <- price_data %>%
    dplyr::filter(date >= start_date)

  if (!is.null(end_date)) {
    filtered_prices <- filtered_prices %>%
      dplyr::filter(date <= end_date)
  }

  ticker_valuation <- build_daily_ev_nopat(filtered_prices, ttm_subset, ticker)
  sector_valuation <- calculate_sector_ev_nopat(filtered_prices, ttm_subset, sector_tickers)
  industry_valuation <- calculate_sector_ev_nopat(filtered_prices, ttm_subset, industry_tickers)

  valuation_data <- ticker_valuation %>%
    dplyr::left_join(
      sector_valuation %>%
        dplyr::select(date, sector_valuation_ratio),
      by = "date"
    ) %>%
    dplyr::left_join(
      industry_valuation %>%
        dplyr::select(date, industry_valuation_ratio = sector_valuation_ratio),
      by = "date"
    )

  list(
    valuation_data = valuation_data,
    ticker = ticker,
    sector_name = sector_name,
    industry_name = industry_name,
    n_sector_stocks = length(sector_tickers),
    n_industry_stocks = length(industry_tickers),
    metric_name = "EV to NOPAT"
  )
}


#' Build Daily EV/NOPAT for Single Ticker
#'
#' @param price_data Daily price data
#' @param ttm_data Quarterly TTM data with per-share columns
#' @param ticker Ticker symbol
#' @return Data frame with daily EV/NOPAT ratio
#' @keywords internal
build_daily_ev_nopat <- function(price_data, ttm_data, ticker) {
  ticker_prices <- price_data %>%
    dplyr::filter(ticker == !!ticker) %>%
    dplyr::select(date, price = adjusted_close) %>%
    dplyr::arrange(date)

  ticker_ttm <- ttm_data %>%
    dplyr::filter(ticker == !!ticker) %>%
    dplyr::select(
      fiscalDateEnding, nopat_per_share,
      debt_total_per_share, lease_obligations_per_share,
      cash_st_investments_per_share, lt_investments_per_share
    ) %>%
    dplyr::rename(date = fiscalDateEnding) %>%
    dplyr::arrange(date)

  ticker_prices %>%
    dplyr::left_join(ticker_ttm, by = "date") %>%
    tidyr::fill(
      nopat_per_share, debt_total_per_share, lease_obligations_per_share,
      cash_st_investments_per_share, lt_investments_per_share,
      .direction = "down"
    ) %>%
    dplyr::filter(!is.na(nopat_per_share) & nopat_per_share > 0) %>%
    dplyr::mutate(
      ev_per_share = avpipeline:::calculate_enterprise_value_per_share(
        price,
        debt_total_per_share,
        lease_obligations_per_share,
        cash_st_investments_per_share,
        lt_investments_per_share
      ),
      valuation_ratio = ev_per_share / nopat_per_share
    )
}


#' Calculate Sector Average EV/NOPAT
#'
#' @param price_data Daily price data
#' @param ttm_data Quarterly TTM data
#' @param sector_tickers Tickers in the sector
#' @return Data frame with daily sector EV/NOPAT ratio
#' @keywords internal
calculate_sector_ev_nopat <- function(price_data, ttm_data, sector_tickers) {
  sector_prices <- price_data %>%
    dplyr::filter(ticker %in% sector_tickers) %>%
    dplyr::select(ticker, date, price = adjusted_close)

  sector_ttm <- ttm_data %>%
    dplyr::filter(ticker %in% sector_tickers) %>%
    dplyr::select(
      ticker, fiscalDateEnding,
      shares = commonStockSharesOutstanding,
      nopat_per_share,
      debt_total_per_share, lease_obligations_per_share,
      cash_st_investments_per_share, lt_investments_per_share
    )

  ticker_list <- unique(sector_prices$ticker)

  daily_fundamentals <- lapply(ticker_list, function(tkr) {
    tkr_prices <- sector_prices %>%
      dplyr::filter(ticker == tkr) %>%
      dplyr::arrange(date)

    tkr_ttm <- sector_ttm %>%
      dplyr::filter(ticker == tkr) %>%
      dplyr::select(-ticker) %>%
      dplyr::rename(date = fiscalDateEnding) %>%
      dplyr::arrange(date)

    tkr_prices %>%
      dplyr::left_join(tkr_ttm, by = "date") %>%
      tidyr::fill(
        shares, nopat_per_share, debt_total_per_share, lease_obligations_per_share,
        cash_st_investments_per_share, lt_investments_per_share,
        .direction = "down"
      ) %>%
      dplyr::mutate(
        ev_per_share = avpipeline:::calculate_enterprise_value_per_share(
          price,
          debt_total_per_share,
          lease_obligations_per_share,
          cash_st_investments_per_share,
          lt_investments_per_share
        ),
        total_ev = ev_per_share * shares,
        total_nopat = nopat_per_share * shares
      )
  }) %>%
    dplyr::bind_rows()

  # Calculate per-ticker valuation ratio, then take median across tickers
  daily_fundamentals %>%
    dplyr::filter(!is.na(ev_per_share) & !is.na(nopat_per_share)) %>%
    dplyr::filter(nopat_per_share > 0) %>%
    dplyr::mutate(ticker_valuation_ratio = ev_per_share / nopat_per_share) %>%
    dplyr::group_by(date) %>%
    dplyr::summarize(
      sector_valuation_ratio = median(ticker_valuation_ratio, na.rm = TRUE),
      .groups = "drop"
    )
}

# Dividends and Buybacks: Cash Flow Statement Data

## Overview

This document captures findings from EDA on dividend and buyback columns in `artifacts$ttm_data`. These columns come from the cash flow statement and are used for calculating shareholder yield metrics.

## Available Columns

### Dividend Columns
- `dividendPayout_ttm` - Total TTM dividend payout (common + preferred)
- `dividendPayoutCommonStock_ttm` - TTM dividends to common shareholders
- `dividendPayoutPreferredStock_ttm` - TTM dividends to preferred shareholders

### Buyback/Issuance Columns
- `proceedsFromRepurchaseOfEquity_ttm` - **Primary buyback column** (see sign conventions below)
- `paymentsForRepurchaseOfEquity_ttm` - Usually 0 in practice
- `paymentsForRepurchaseOfCommonStock_ttm` - Usually 0 in practice
- `paymentsForRepurchaseOfPreferredStock_ttm` - Usually 0 in practice
- `proceedsFromIssuanceOfCommonStock_ttm` - Stock issuance (dilution)
- `proceedsFromSaleOfTreasuryStock_ttm` - Treasury stock sales

## Sign Conventions

### Dividends
- **Always positive** - Cash outflow to shareholders
- `dividendPayout_ttm` typically equals `dividendPayoutCommonStock_ttm` for companies without preferred stock

### Buybacks (`proceedsFromRepurchaseOfEquity_ttm`)
This column has **mixed sign conventions** across tickers:

| Sign | Count | Meaning |
|------|-------|---------|
| Negative | 1,482 | Actual buybacks (cash outflow spent on repurchases) |
| Positive | 299 | Likely equity issuance mis-classified or accounting quirk |
| Zero | 225 | No buyback activity |

**Key insight**: Use `pmax(-proceedsFromRepurchaseOfEquity_ttm, 0)` to safely extract only actual buybacks.

## Specific Examples

### AAPL (Large Buyback Program)
```
fiscalDateEnding  dividendPayout_ttm  proceedsFromRepurchaseOfEquity_ttm
2025-09-30        15,234,000,000      -90,711,000,000
2025-06-30        15,176,000,000      -95,662,000,000
2025-03-31        15,313,000,000      -98,731,000,000
2024-12-31        15,265,000,000      -96,038,000,000
```
- Dividends: ~$15B TTM (positive = cash paid out)
- Buybacks: ~$90-98B TTM (negative = cash spent on repurchases)
- `dividendPayoutCommonStock_ttm` equals `dividendPayout_ttm` (no preferred)

### MSFT (Consistent Buybacks)
```
fiscalDateEnding  dividendPayout_ttm  proceedsFromRepurchaseOfEquity_ttm
2025-09-30        24,081,000,000      -19,963,000,000
2025-06-30        23,486,000,000      -18,420,000,000
2025-03-31        22,891,000,000      -18,084,000,000
2024-12-31        22,294,000,000      -17,516,000,000
```
- Dividends: ~$22-24B TTM
- Buybacks: ~$18-20B TTM (negative values = actual buybacks)

### TSLA (Positive Buyback Value - Anomaly)
```
fiscalDateEnding  proceedsFromRepurchaseOfEquity_ttm  dividendPayout_ttm
2025-09-30        2,609,000,000                       254,008,000
2025-06-30        2,297,000,000                       254,008,000
```
- Shows **positive** `proceedsFromRepurchaseOfEquity_ttm` (~$2.5B)
- This is anomalous - likely represents equity issuance or different accounting treatment
- For yield calculations, this should NOT be treated as a buyback

## Recommended Calculation Formulas

### For Common Shareholder Perspective

```r
# Dividends (simple - use total or common-specific)
dividends_ttm <- dividendPayout_ttm
# or for common-only:
dividends_ttm <- dividendPayoutCommonStock_ttm

# Buybacks (safe extraction - only negative values = actual buybacks)
buybacks_ttm <- pmax(-proceedsFromRepurchaseOfEquity_ttm, 0)

# Per-share values
dividend_per_share <- dividends_ttm / commonStockSharesOutstanding
buyback_per_share <- buybacks_ttm / commonStockSharesOutstanding

# Yields
dividend_yield <- dividend_per_share / price
buyback_yield <- buyback_per_share / price
shareholder_yield <- dividend_yield + buyback_yield
```

### Why Dollar-Based (Not Share Count) for Buyback Yield

We use dollar-based buyback yield (`buybacks / market_cap`) rather than share count change because:
1. Consistent with dividend yield calculation (both dollar-based, additive)
2. Measures actual cash returned to shareholders
3. Industry standard for shareholder yield metrics

The share count approach (`(prior_shares - current_shares) / prior_shares`) captures net dilution but isn't additive with dollar-based dividend yield.

## EDA Guidance

To explore this data yourself:

### 1. Load the Data
```r
library(dplyr)
devtools::load_all()
artifacts <- get_cached_artifacts()
ttm_data <- artifacts$ttm_data
```

### 2. Explore Column Names
```r
# All dividend/buyback related columns
ttm_data %>%
  colnames() %>%
  grep("dividend|repurchase|issuance|treasury", ., value = TRUE, ignore.case = TRUE)
```

### 3. Check Sign Distribution
```r
# How many tickers have positive vs negative buyback values?
ttm_data %>%
  group_by(ticker) %>%
  filter(fiscalDateEnding == max(fiscalDateEnding)) %>%
  ungroup() %>%
  mutate(
    buyback_sign = case_when(
      proceedsFromRepurchaseOfEquity_ttm > 0 ~ "positive",
      proceedsFromRepurchaseOfEquity_ttm < 0 ~ "negative",
      proceedsFromRepurchaseOfEquity_ttm == 0 ~ "zero",
      TRUE ~ "NA"
    )
  ) %>%
  count(buyback_sign)
```

### 4. Examine Specific Tickers
```r
# Pick a known buyback company
ttm_data %>%
  filter(ticker == "AAPL") %>%
  arrange(desc(fiscalDateEnding)) %>%
  head(8) %>%
  select(
    fiscalDateEnding,
    dividendPayout_ttm,
    dividendPayoutCommonStock_ttm,
    dividendPayoutPreferredStock_ttm,
    proceedsFromRepurchaseOfEquity_ttm,
    paymentsForRepurchaseOfEquity_ttm,
    proceedsFromIssuanceOfCommonStock_ttm,
    commonStockSharesOutstanding
  )
```

### 5. Find Anomalies
```r
# Companies with positive "repurchase" values (investigate these)
ttm_data %>%
  filter(proceedsFromRepurchaseOfEquity_ttm > 100000000) %>%
  distinct(ticker) %>%
  head(20)
```

### 6. Validate Against Known Data
Cross-reference with company investor relations or SEC filings to validate the data makes sense for specific tickers you care about.

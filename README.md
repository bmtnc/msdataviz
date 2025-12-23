# msdataviz

Data visualization and analytics package for financial data produced by [avpipeline](https://github.com/bmtnc/avpipeline).

## S3 Data Location

All data lives in: `s3://avpipeline-artifacts-prod/`

### Bucket Structure

```
avpipeline-artifacts-prod/
├── raw/{TICKER}/                    # Raw data per ticker
│   ├── balance_sheet.parquet
│   ├── cash_flow.parquet
│   ├── earnings.parquet
│   ├── income_statement.parquet
│   ├── overview.parquet
│   ├── price.parquet
│   ├── splits.parquet
│   └── ttm.parquet                  # Processed TTM data for this ticker
├── ttm-artifacts/{DATE}/            # Full ETF artifacts by run date
│   └── ttm_per_share_financial_artifact.parquet
├── checkpoint/                      # Pipeline checkpoints
└── logs/                            # Pipeline run logs
```

### What's Available

| Path | Description | Use Case |
|------|-------------|----------|
| `raw/{TICKER}/ttm.parquet` | Single ticker TTM data | Analyze individual stocks |
| `raw/{TICKER}/price.parquet` | Raw daily prices | Price-only analysis |
| `ttm-artifacts/{DATE}/ttm_per_share_financial_artifact.parquet` | All tickers from ETF run | Cross-sectional analysis |

## Reading Data from S3

### Single Ticker (Recommended)

```r
# Read processed TTM data for a single ticker
ttm_data <- arrow::read_parquet(

  "s3://avpipeline-artifacts-prod/raw/AAPL/ttm.parquet"
)

# Or use avpipeline helper (processes raw data on-the-fly)
ttm_data <- avpipeline::process_ticker_from_s3(

  ticker = "AAPL",
  bucket_name = "avpipeline-artifacts-prod"
)
```

### Full ETF Artifact

```r
# Read the latest full artifact (all tickers from last pipeline run)
ttm_all <- arrow::read_parquet(

  "s3://avpipeline-artifacts-prod/ttm-artifacts/2025-12-23/ttm_per_share_financial_artifact.parquet"
)

# Filter to specific ticker
aapl <- ttm_all |> dplyr::filter(ticker == "AAPL")
```

### Raw Financial Statements

```r
# Read raw balance sheet
balance_sheet <- arrow::read_parquet(
  "s3://avpipeline-artifacts-prod/raw/AAPL/balance_sheet.parquet"
)

# Read raw income statement
income <- arrow::read_parquet(
  "s3://avpipeline-artifacts-prod/raw/AAPL/income_statement.parquet"
)
```

## Key Columns

### Date Columns

| Column | Description | Use For |
|--------|-------------|---------|
| `date` | Daily price date | Primary key for daily analysis |
| `fiscalDateEnding` | Quarter end date | Quarterly KPI analysis |
| `reportedDate` | Actual earnings announcement | When financials became public |

### Price & Market Data

| Column | Description |
|--------|-------------|
| `adjusted_close` | Split/dividend-adjusted close price |
| `close` | Unadjusted close price |
| `volume` | Daily trading volume |
| `market_cap` | Daily market capitalization |

### Share Metrics

| Column | Description |
|--------|-------------|
| `commonStockSharesOutstanding` | Shares outstanding (from filings) |
| `effective_shares_outstanding` | Split-adjusted shares |

### TTM Per-Share Metrics (Flow - Income/Cash Flow)

| Column | Description |
|--------|-------------|
| `totalRevenue_ttm_per_share` | Revenue per share (TTM) |
| `grossProfit_ttm_per_share` | Gross profit per share (TTM) |
| `operatingIncome_ttm_per_share` | Operating income per share (TTM) |
| `netIncome_ttm_per_share` | Net income per share (TTM) |
| `ebitda_ttm_per_share` | EBITDA per share (TTM) |
| `operatingCashflow_ttm_per_share` | Operating cash flow per share (TTM) |
| `capitalExpenditures_ttm_per_share` | CapEx per share (TTM) |

### Derived Metrics

| Column | Description |
|--------|-------------|
| `nopat_ttm_per_share` | Net Operating Profit After Tax per share |
| `fcf_ttm_per_share` | Free Cash Flow per share |
| `invested_capital_per_share` | Invested capital per share |
| `enterprise_value_per_share` | Enterprise value per share |

### Balance Sheet Per-Share (Point-in-Time)

| Column | Description |
|--------|-------------|
| `totalAssets_per_share` | Total assets per share |
| `totalLiabilities_per_share` | Total liabilities per share |
| `totalShareholderEquity_per_share` | Book value per share |
| `totalDebt_per_share` | Total debt per share |
| `cashAndShortTermInvestments_per_share` | Cash per share |

## Common Patterns

### Calculate Valuation Multiples

```r
data <- data |>
  dplyr::mutate(
    ev_ebitda = enterprise_value_per_share / ebitda_ttm_per_share,
    ev_nopat = enterprise_value_per_share / nopat_ttm_per_share,
    ev_fcf = enterprise_value_per_share / fcf_ttm_per_share,
    pe_ratio = adjusted_close / netIncome_ttm_per_share,
    price_to_book = adjusted_close / totalShareholderEquity_per_share
  )
```

### Calculate ROIC

```r
data <- data |>
  dplyr::mutate(
    roic = nopat_ttm_per_share / invested_capital_per_share * 100
  )
```

### Filter to Quarterly Frequency

```r
# Get one row per quarter (for bar charts, etc.)
quarterly <- data |>
  dplyr::distinct(fiscalDateEnding, .keep_all = TRUE) |>
  dplyr::arrange(fiscalDateEnding)
```

### Get Latest Values

```r
latest <- data |>
  dplyr::filter(date == max(date))
```

## Scripts

| Script | Description |
|--------|-------------|
| `scripts/price_decomp_single_stock.R` | Decompose price changes into fundamental vs multiple |
| `scripts/explore_single_stock.R` | Dual-plot explorer: quarterly KPI + daily valuation |

Both scripts support `DATA_SOURCE = "s3"` or `DATA_SOURCE = "api"`.

## Installation

```r
# Install dependencies
renv::restore()

# Or install avpipeline directly
renv::install("bmtnc/avpipeline")
```

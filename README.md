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
│   └── splits.parquet
├── ttm-artifacts/{DATE}/            # Combined artifacts by run date
│   ├── ttm_quarterly_artifact.parquet   # Quarterly financials + TTM metrics
│   └── price_artifact.parquet           # Daily prices for all tickers
└── logs/                            # Pipeline run logs
```

### What's Available

| Artifact | Frequency | Description |
|----------|-----------|-------------|
| `ttm_quarterly_artifact.parquet` | Quarterly | Financial statements with TTM metrics |
| `price_artifact.parquet` | Daily | Adjusted prices, volume, splits |

## Architecture Notes

### Why Two Artifacts?

The price artifact is **daily** (~250 rows/ticker/year) while TTM is **quarterly** (~4 rows/ticker/year). Keeping them separate avoids a 60x data explosion. Join on-demand when needed.

### Which Loader to Use

| Use Case | Function | Notes |
|----------|----------|-------|
| Price analysis (returns, drawdowns) | `get_latest_price_artifact()` | Daily frequency, lightweight |
| Fundamental snapshots | `get_latest_ttm_artifact()` | Quarterly frequency, use for screening |
| Valuation over time (P/E, P/S) | `load_daily_ttm_artifact()` | Joins + ffills quarterly→daily |

### Memory Considerations

When joining TTM to daily prices, **select only needed columns first**:

```r
# GOOD: Select before join
ttm_subset <- avpipeline::get_latest_ttm_artifact() |>
  dplyr::select(ticker, fiscalDateEnding, sector, netIncome_ttm)

# BAD: Join full TTM artifact (50+ columns × daily rows)
daily <- avpipeline::load_daily_ttm_artifact()  # Large object
```

### Sector/Industry for Base Rates

The TTM artifact includes `sector` and `industry` columns for grouping. Use these to calculate sector averages as comparison benchmarks:

```r
ttm <- avpipeline::get_latest_ttm_artifact()

# Get sector for a ticker
target_sector <- ttm |>
  dplyr::filter(ticker == "AAPL") |>
  dplyr::pull(sector) |>
  unique()

# Filter to sector peers
sector_peers <- ttm |>
  dplyr::filter(sector == target_sector)
```

## Reading Data from S3

### Load Latest Artifacts (Recommended)

```r
# Load quarterly TTM data (all tickers)
ttm <- avpipeline::get_latest_ttm_artifact()

# Load daily price data (all tickers)
prices <- avpipeline::get_latest_price_artifact()

# Filter to specific ticker
aapl <- ttm |> dplyr::filter(ticker == "AAPL")
```

### Add Per-Share Metrics

The TTM artifact contains raw financials. Use `add_per_share_columns()` to create per-share metrics:

```r
ttm <- avpipeline::get_latest_ttm_artifact()

# Add per-share columns for selected metrics
ttm <- ttm |>
  avpipeline::add_per_share_columns(
    cols = c("totalRevenue_ttm", "netIncome_ttm", "operatingCashflow_ttm",
             "ebitda_ttm", "totalAssets", "totalShareholderEquity")
  )

# Now you have: totalRevenue_ttm_per_share, netIncome_ttm_per_share, etc.
```

### Daily Frequency with Per-Share Metrics

For daily-frequency analysis with forward-filled financials and per-share metrics:

```r
# Load and join both artifacts, calculate per-share metrics
daily <- avpipeline::load_daily_ttm_artifact(
  bucket_name = "avpipeline-artifacts-prod"
)

# Or filter to specific tickers/dates
daily <- avpipeline::load_daily_ttm_artifact(
 bucket_name = "avpipeline-artifacts-prod",
  tickers = c("AAPL", "MSFT"),
  start_date = as.Date("2020-01-01")
)
```

## Key Columns

### TTM Quarterly Artifact

#### Identifiers & Dates

| Column | Description |
|--------|-------------|
| `ticker` | Stock ticker symbol |
| `fiscalDateEnding` | Quarter end date (use for quarterly analysis) |
| `reportedDate` | Actual earnings announcement date |
| `calendar_quarter_ending` | Standardized calendar quarter |

#### Share Data

| Column | Description |
|--------|-------------|
| `commonStockSharesOutstanding` | Shares outstanding from balance sheet |

#### TTM Flow Metrics (Income/Cash Flow)

| Column | Description |
|--------|-------------|
| `totalRevenue_ttm` | Revenue (trailing 12 months) |
| `grossProfit_ttm` | Gross profit (TTM) |
| `operatingIncome_ttm` | Operating income (TTM) |
| `netIncome_ttm` | Net income (TTM) |
| `ebitda_ttm` | EBITDA (TTM) |
| `operatingCashflow_ttm` | Operating cash flow (TTM) |
| `capitalExpenditures_ttm` | Capital expenditures (TTM) |

#### Balance Sheet (Point-in-Time)

| Column | Description |
|--------|-------------|
| `totalAssets` | Total assets |
| `totalLiabilities` | Total liabilities |
| `totalShareholderEquity` | Book value |
| `longTermDebt` | Long-term debt |
| `cashAndShortTermInvestments` | Cash and equivalents |

#### Metadata

| Column | Description |
|--------|-------------|
| `sector` | Company sector |
| `industry` | Company industry |
| `exchange` | Stock exchange |

### Price Artifact

| Column | Description |
|--------|-------------|
| `ticker` | Stock ticker symbol |
| `date` | Trading date |
| `adjusted_close` | Split/dividend-adjusted close price |
| `volume` | Daily trading volume |
| `split_coefficient` | Split ratio (1.0 = no split) |

## Common Patterns

### Calculate Per-Share Metrics

```r
ttm <- avpipeline::get_latest_ttm_artifact() |>
  avpipeline::add_per_share_columns(
    cols = c("totalRevenue_ttm", "netIncome_ttm", "ebitda_ttm",
             "operatingCashflow_ttm", "capitalExpenditures_ttm",
             "totalAssets", "totalShareholderEquity", "longTermDebt")
  ) |>
  dplyr::mutate(
    # Derived metrics
    fcf_ttm = operatingCashflow_ttm - capitalExpenditures_ttm,
    fcf_ttm_per_share = fcf_ttm / commonStockSharesOutstanding
  )
```

### Get Latest Quarter for Each Ticker

```r
latest <- ttm |>
  dplyr::group_by(ticker) |>
  dplyr::filter(fiscalDateEnding == max(fiscalDateEnding)) |>
  dplyr::ungroup()
```

### Join Price and Quarterly Data

```r
# Get latest price for each ticker
latest_prices <- prices |>
  dplyr::group_by(ticker) |>
  dplyr::filter(date == max(date)) |>
  dplyr::ungroup() |>
  dplyr::select(ticker, date, adjusted_close)

# Join with latest quarterly data
combined <- latest |>
  dplyr::inner_join(latest_prices, by = "ticker")
```

### Calculate Valuation Multiples

```r
# After adding per-share columns and joining with prices
data <- data |>
  dplyr::mutate(
    pe_ratio = adjusted_close / netIncome_ttm_per_share,
    price_to_book = adjusted_close / totalShareholderEquity_per_share,
    price_to_sales = adjusted_close / totalRevenue_ttm_per_share
  )
```

## Scripts

| Script | Description |
|--------|-------------|
| `scripts/price_decomp_single_stock.R` | Decompose price changes into fundamental vs multiple |
| `scripts/explore_single_stock.R` | Dual-plot explorer: quarterly KPI + daily valuation |

## Installation

```r
# Install dependencies
renv::restore()

# Or install avpipeline directly
renv::install("bmtnc/avpipeline")
```

## Deployment

### How it works

The app runs in Docker on a Mac Mini and is accessible via the internet through a Cloudflare Tunnel.

```
Someone visits unfixedincome.net
        ↓
Cloudflare receives the request
        ↓
Sends it through an encrypted tunnel to your Mac Mini
        ↓
cloudflared (running on Mac Mini) forwards to localhost:3838
        ↓
Docker container runs the Shiny app
        ↓
Response goes back through the tunnel to the visitor
```

The key insight: your Mac Mini reaches *out* to Cloudflare (not the other way around). This means no ports need to be opened on your router, and your home IP address stays hidden.

### Components

1. **Docker** - Runs the Shiny app in an isolated container
2. **cloudflared** - A small program that maintains the tunnel connection to Cloudflare
3. **Cloudflare** - Routes traffic from your domain to your tunnel

### Docker setup

Build and run the container:

```bash
docker compose build   # Takes ~30 min first time (compiles R packages)
docker compose up -d   # Start container in background
docker compose logs -f # View logs
docker compose down    # Stop container
```

The `docker-compose.yml` mounts your `~/.aws` credentials so the app can fetch data from S3.

### Cloudflare Tunnel setup

One-time setup (already done):

```bash
# Install the tunnel client
brew install cloudflared

# Login to Cloudflare (opens browser)
cloudflared tunnel login

# Create a named tunnel
cloudflared tunnel create msdataviz

# Route your domain to the tunnel
cloudflared tunnel route dns msdataviz unfixedincome.net
```

The config file at `~/.cloudflared/config.yml` tells the tunnel where to forward traffic:

```yaml
tunnel: <tunnel-id>
credentials-file: /Users/barrymatanic/.cloudflared/<tunnel-id>.json

ingress:
  - hostname: unfixedincome.net
    service: http://localhost:3838
  - service: http_status:404
```

### Running the tunnel

Manual start:
```bash
cloudflared tunnel run msdataviz
```

Auto-start on boot (recommended):
```bash
brew services start cloudflared
```

### Verification

1. Start Docker: `docker compose up -d`
2. Start tunnel: `cloudflared tunnel run msdataviz`
3. Visit https://unfixedincome.net

### Troubleshooting

```bash
# Check if container is running
docker ps

# View container logs
docker compose logs -f

# Check tunnel status
cloudflared tunnel info msdataviz

# Test locally (bypass tunnel)
open http://localhost:3838
```

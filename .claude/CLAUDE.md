# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with R code in this repository.

## R Style Guide

### Core Principles

1. **Tidyverse code style** - Use `dplyr`, pipes (`%>%`), and tidyverse conventions
2. **Declarative over imperative** - Express what you want, not how to get it
3. **Vectorization strongly preferred** - Avoid nested operations and `for` loops when possible
4. **Explicit namespacing** - Always use `package::function()` for third-party packages

### Explicit Namespacing

**Always use explicit namespacing for third-party packages.** Never use `library()` and bare function calls.

```r
# GOOD: Explicit namespacing
result <- data %>%
  dplyr::filter(price > 0) %>%
  dplyr::mutate(log_price = log(price)) %>%
  tibble::as_tibble()

# BAD: Implicit imports
library(dplyr)
library(tibble)
result <- data %>%
  filter(price > 0) %>%
  mutate(log_price = log(price)) %>%
  as_tibble()
```

**Do NOT namespace base R or default packages:**
- Base R functions: `log()`, `exp()`, `mean()`, `sqrt()`, `sum()`, `range()`, etc.
- Default packages: `stats::`, `utils::`, `graphics::`

```r
# GOOD: Base R without namespace, third-party with namespace
result <- dplyr::mutate(data,
  log_return = log(price / dplyr::lag(price)),  # log() is base R, lag() is dplyr
  z_score = (return - mean(return)) / sd(return)  # mean/sd are base R
)

# BAD: Unnecessary base:: prefix
result <- dplyr::mutate(data,
  log_return = base::log(price / base::lag(price))
)
```

**This applies to all third-party packages:**
- `dplyr::`, `tibble::`, `tidyr::`
- `ggplot2::`, `lubridate::`, `stringr::`
- `purrr::`, `forcats::`, `readr::`
- Any package not included with base R installation

### Grouped Operations: group_by + mutate (Preferred)

Use `dplyr::group_by()` with `dplyr::mutate()` for grouped operations:

```r
# GOOD: Group-by + mutate pattern
result <- data %>%
  dplyr::arrange(id, date) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    lag_value = dplyr::lag(value),
    pct_change = (value / lag_value - 1) * 100
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(pct_change))

# BAD: Unnecessary split-apply-bind
result <- data %>%
  split(.$id) %>%
  lapply(function(df) {
    df %>% dplyr::mutate(pct_change = (value / dplyr::lag(value) - 1) * 100)
  }) %>%
  dplyr::bind_rows()
```

**Use split-apply-bind only as a last resort** when operations truly cannot be expressed in `group_by() + mutate()`.

### Vectorization Over Loops

**Strongly prefer vectorized operations:**

```r
# GOOD: Vectorized operations
z_scores <- (values - mean(values)) / sd(values)
scaled <- values * scaling_factors  # Element-wise multiplication
distances <- sqrt(rowSums((predicted - actual)^2))

# BAD: Loops
z_scores <- numeric(length(values))
for (i in seq_along(values)) {
  z_scores[i] <- (values[i] - mean(values)) / sd(values)
}
```

**Acceptable loop usage:**
- Sequential dependencies (e.g., running calculations that depend on previous rows)
- Progress reporting in long computations
- Truly non-vectorizable operations

### Declarative Style

```r
# GOOD: Declarative
top_items <- data %>%
  dplyr::filter(date == latest_date) %>%
  dplyr::arrange(dplyr::desc(value)) %>%
  dplyr::slice_head(n = 10)

# BAD: Imperative with loops
top_items <- data.frame()
for (i in 1:nrow(data)) {
  if (data$date[i] == latest_date) {
    top_items <- rbind(top_items, data[i, ])
  }
}
top_items <- top_items[order(-top_items$value), ][1:10, ]
```

## Function Design Principles

### Single Responsibility Principle

Each function should do one thing and do it well. Functions that try to do too many things should be split into smaller, focused helper functions.

```r
# BAD: Function trying to do too much
process_data <- function(data, threshold) {
  # 1. Filter data
  data <- data %>%
    dplyr::filter(value > threshold)
  # 2. Calculate metrics
  data <- dplyr::mutate(data,
    pct_change = (value / dplyr::lag(value) - 1) * 100
  )
  # 3. Validate results
  if (nrow(data) == 0) stop("No data after filtering")
  data
}

# GOOD: Separated into focused functions
filter_data <- function(data, threshold) {
  # Only filter
  data %>%
    dplyr::filter(value > threshold)
}

calculate_pct_change <- function(data) {
  # Only calculate one metric
  data %>%
    dplyr::mutate(pct_change = (value / dplyr::lag(value) - 1) * 100)
}

validate_data <- function(data) {
  # Only validate
  if (nrow(data) == 0) stop("No data after filtering")
  data
}

# Compose functions for the full pipeline
process_data <- function(data, threshold) {
  filter_data(data, threshold) %>%
    calculate_pct_change() %>%
    validate_data()
}
```

**Benefits of this approach:**
- Each function is simple and easy to understand
- Each function is independently testable
- Functions can be reused in different pipelines
- Easier to debug and maintain
- Pipelines clearly express intent

## Function Naming

- Use snake_case throughout
- Use action verbs: `calculate_`, `prepare_`, `extract_`, `evaluate_`, `infer_`, `normalize_`, `detect_`, `clean_`, `align_`, `join_`, `filter_`
- Be descriptive but concise
- Example: `calculate_summary_statistics()`, `prepare_data_for_model()`

### Two-Tier Calculation Functions

Financial calculations follow a two-tier pattern:

**Tier 1: Vectorized functions (no prefix)**
- Pure mathematical operations on vectors
- Simple, testable building blocks
- Named as nouns/concepts, not actions

```r
# GOOD: Vectorized function - operates on vectors, returns vector/list
cumulative_return <- function(prices) {
  prices / prices[1] - 1
}

distribute_interaction <- function(contrib_a, contrib_b, interaction) {
  total <- abs(contrib_a) + abs(contrib_b)
  list(
    a_adjusted = contrib_a + ifelse(total > 0, interaction * abs(contrib_a) / total, interaction / 2),
    b_adjusted = contrib_b + ifelse(total > 0, interaction * abs(contrib_b) / total, interaction / 2)
  )
}
```

**Tier 2: DataFrame wrappers (`calculate_` prefix)**
- "df in, df out" functions
- Call vectorized functions internally
- Handle column selection and mutation

```r
# GOOD: DataFrame wrapper - calls vectorized function
calculate_cumulative_return <- function(data, price_col = "price") {
  data %>%
    dplyr::mutate(cumulative_return = cumulative_return(.data[[price_col]]))
}

calculate_price_decomposition <- function(data, base_date) {
  # Uses distribute_interaction() and other vectorized helpers
  # Returns df with decomposition columns
}
```

**Benefits:**
- Vectorized functions are easy to unit test with simple inputs
- DataFrame wrappers handle the plumbing
- Clear naming convention signals function type

## Package Development Standards

### Documentation

- All functions need Roxygen2 documentation with `@param`, `@return`, `@keywords internal`
- Keep documentation **CONCISE** - prefer one-line descriptions
- Code should be self-documenting - detailed steps should be read from the code itself
- Good: "Filters and transforms raw data"
- Bad: Verbose multi-paragraph descriptions listing all steps

### Testing

- Every function needs comprehensive tests in `tests/testthat/test_<function_name>.R`
- Test edge cases: empty inputs, NA values, invalid parameters
- Test parameter validation with specific error messages
- Test mathematical properties against known results when possible
- Use `testthat::expect_error()` for invalid inputs
- Aim for comprehensive coverage of all code paths

### Code Organization

```
R/
├── function_name_1.R
├── function_name_2.R
├── helper_function_1.R
├── helper_function_2.R

tests/testthat/
└── test_function_name_1.R
```

**One function per file** - this is a hard rule. Every function, including internal helpers, must be in its own file named after the function (e.g., `validate_plot_params.R` for `validate_plot_params()`).

## Common Development Commands

```r
# Load package functions during development
devtools::load_all()

# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test_function_name.R")

# Check package
devtools::check()

# Build documentation
devtools::document()
```

## Dependency Management

Use `renv` for reproducible dependency management:

```r
# Restore exact package versions
renv::restore()

# Install or update a package
renv::install("packagename")

# Update lockfile after changes
renv::snapshot()
```

## Validation Functions

**Always use `avpipeline::validate_*` functions** - never write custom validation logic when avpipeline provides a validator.

Available validators:
- `avpipeline::validate_df_cols(data, required_cols)` - validates data.frame type and required columns
- `avpipeline::validate_non_empty(x, name)` - validates object is not NULL/empty (df has rows)
- `avpipeline::validate_character_scalar(x, allow_empty, name)` - validates single character string
- `avpipeline::validate_positive(x, name)` - validates positive numeric value
- `avpipeline::validate_numeric_scalar(x, name)` - validates single numeric value
- `avpipeline::validate_date_type(x, name)` - validates Date type

```r
# GOOD: Use avpipeline validators
plot_data <- function(data, ticker) {

avpipeline::validate_df_cols(data, c("date", "value"))
avpipeline::validate_non_empty(data, "data")
avpipeline::validate_character_scalar(ticker, allow_empty = FALSE, name = "ticker")
# ...
}

# BAD: Writing custom validation
plot_data <- function(data, ticker) {
if (!is.data.frame(data)) stop("data must be a data frame")
if (nrow(data) == 0) stop("data cannot be empty")
# ...
}
```

## Data Visualization Conventions

### Global Theme

Call `set_ggplot_theme()` once before creating plots. This sets a global theme with our conventions. Plot functions (`plot_*`) should **not** include `theme_minimal()` or duplicate theme settings - they inherit from the global theme.

```r
# In scripts or Rmd setup chunks
set_ggplot_theme()

# Plot functions just define geoms, scales, and labs - no theme infrastructure
plot_price <- function(data, ticker) {
  data %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = price)) +
    ggplot2::geom_line(color = "steelblue") +
    ggplot2::labs(title = paste0(ticker, " Price"), x = NULL, y = "Price")
    # No theme_minimal() or theme() - inherits from global
}
```

### Chart Style Rules

1. **No vertical gridlines** - Only horizontal gridlines (major and minor x removed)
2. **No "Date" axis label** - Let the reader infer date axes; use `x = NULL`
3. **Flat date labels** - Never tilt/angle; use `angle = 0, hjust = 0.5`

## Artifacts Structure

The `get_cached_artifacts()` function returns preprocessed data. Key columns in `artifacts$ttm_data`:

- `fiscalDateEnding` - Company's actual fiscal period end date (varies by company)
- `calendar_quarter_ending` - Normalized to standard quarter ends (Mar 31, Jun 30, Sep 30, Dec 31)
- `sector`, `subsector`, `industry` - Classification hierarchy
- `*_ttm` columns - Trailing twelve month aggregates

**Use `calendar_quarter_ending` for cross-sectional comparisons** since companies have different fiscal calendars. Joining on `fiscalDateEnding` will fail to match peers.

## Docker Workflow

**NEVER use `--no-cache` when rebuilding Docker containers** unless explicitly requested. Docker layer caching exists for a reason - it preserves expensive operations like installing R packages.

```bash
# GOOD: Let Docker use cached layers
docker compose build shiny
docker compose up -d

# BAD: Forces full rebuild, reinstalls all packages unnecessarily
docker compose build --no-cache shiny
```

For R code changes, only the final `COPY` layer needs to rebuild. Using `--no-cache` wastes significant time reinstalling all dependencies.

**You almost never need `--no-cache`:**
- New R dependencies: Docker detects `renv.lock` changed and rebuilds from that layer
- Code changes: Docker detects source files changed and rebuilds final layers
- Only use `--no-cache` if: base image needs updating, cache is corrupted, or user explicitly requests it

## Important Notes

- Avoid backwards-compatibility hacks or re-exports of unused types
- If something is unused, delete it completely
- Don't add comments to code you didn't change
- Only add comments where the logic isn't self-evident
- Avoid over-engineering - make changes directly requested or clearly necessary
- Don't add error handling or validation for scenarios that can't happen

#' Get Cached Network Data from S3
#'
#' Loads customer-supplier relationship data with local caching.
#' Re-fetches from S3 if cache is stale or missing.
#'
#' @param cache_dir Directory for cache files (default: ~/.cache/msdataviz)
#' @param max_age_days Maximum cache age in days before refresh (default: 5)
#' @param force_refresh Force re-fetch from S3 regardless of cache age
#' @return Data frame with ticker, counterparty_ticker, relationship columns
#' @export
get_cached_network_data <- function(
    cache_dir = "~/.cache/msdataviz",
    max_age_days = 5,
    force_refresh = FALSE
) {
  avpipeline::validate_character_scalar(cache_dir, allow_empty = FALSE, name = "cache_dir")
  avpipeline::validate_positive(max_age_days, name = "max_age_days")

  s3_uri <- "s3://ms-filings/msnetwork/final/matched.parquet"

  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  data_cache <- file.path(cache_dir, "network_data.rds")
  meta_cache <- file.path(cache_dir, "network_cache_meta.rds")

  # Check if cache is fresh
  cache_is_fresh <- FALSE
  cache_age_days <- NA
  if (!force_refresh && file.exists(meta_cache)) {
    meta <- readRDS(meta_cache)
    cache_age_days <- as.numeric(difftime(Sys.time(), meta$cached_at, units = "days"))
    cache_is_fresh <- cache_age_days < max_age_days
  }

  if (cache_is_fresh && file.exists(data_cache)) {
    message("Loading network data from cache (", round(cache_age_days, 1), " days old)")
    return(readRDS(data_cache))
  }

  # Fetch from S3 (uses AWS credentials from environment)
  message("Fetching network data from S3...")
  network_data <- arrow::read_parquet(s3_uri)

  # Save to cache
  saveRDS(network_data, data_cache)
  saveRDS(list(cached_at = Sys.time()), meta_cache)
  message("Network data cached to ", cache_dir)

  network_data
}

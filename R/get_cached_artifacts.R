#' Get Cached Artifacts from S3
#'
#' Loads price and TTM artifacts with local caching. Re-fetches from S3
#' if cache is stale or missing.
#'
#' @param cache_dir Directory for cache files (default: ~/.cache/msdataviz)
#' @param max_age_days Maximum cache age in days before refresh (default: 5)
#' @param s3_bucket S3 bucket name
#' @param aws_region AWS region
#' @param force_refresh Force re-fetch from S3 regardless of cache age
#' @return List with price_data and ttm_data tibbles
#' @export
get_cached_artifacts <- function(
    cache_dir = "~/.cache/msdataviz",
    max_age_days = 5,
    s3_bucket = Sys.getenv("S3_BUCKET", "avpipeline-artifacts-prod"),
    aws_region = Sys.getenv("AWS_REGION", "us-east-1"),
    force_refresh = FALSE
) {
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  price_cache <- file.path(cache_dir, "price_artifact.rds")
  ttm_cache <- file.path(cache_dir, "ttm_artifact.rds")
  meta_cache <- file.path(cache_dir, "cache_meta.rds")

  # Check if cache is fresh
  cache_is_fresh <- FALSE
  if (!force_refresh && file.exists(meta_cache)) {
    meta <- readRDS(meta_cache)
    cache_age_days <- as.numeric(difftime(Sys.time(), meta$cached_at, units = "days"))
    cache_is_fresh <- cache_age_days < max_age_days
  }

  if (cache_is_fresh && file.exists(price_cache) && file.exists(ttm_cache)) {
    message("Loading artifacts from cache (", round(cache_age_days, 1), " days old)")
    return(list(
      price_data = readRDS(price_cache),
      ttm_data = readRDS(ttm_cache)
    ))
  }

  # Fetch from S3

  message("Fetching artifacts from S3...")
  price_data <- avpipeline::get_latest_price_artifact(
    bucket_name = s3_bucket,
    region = aws_region
  )

  ttm_data <- avpipeline::get_latest_ttm_artifact(
    bucket_name = s3_bucket,
    region = aws_region
  )


  # Enrich TTM data with subsector and standardize to snake_case
  ttm_data <- join_equities_taxonomy(ttm_data)

  # Save to cache

  saveRDS(price_data, price_cache)
  saveRDS(ttm_data, ttm_cache)
  saveRDS(list(cached_at = Sys.time()), meta_cache)
  message("Artifacts cached to ", cache_dir)

  list(
    price_data = price_data,
    ttm_data = ttm_data
  )
}


#' Clear Artifact Cache
#'
#' Removes cached artifact files.
#'
#' @param cache_dir Directory for cache files (default: ~/.cache/msdataviz)
#' @return Invisible NULL
#' @export
clear_artifact_cache <- function(cache_dir = "~/.cache/msdataviz") {
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)
    message("Cache cleared: ", cache_dir)
  } else {
    message("No cache found at: ", cache_dir)
  }

  invisible(NULL)
}

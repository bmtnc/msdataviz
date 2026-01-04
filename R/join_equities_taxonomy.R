#' Join Equities Taxonomy to TTM Data
#'
#' Left joins subsector taxonomy onto TTM artifact by industry.
#'
#' @param ttm_data TTM artifact tibble with `industry` column
#' @return TTM data with `subsector` column added
#' @export
join_equities_taxonomy <- function(ttm_data) {
  avpipeline::validate_df_cols(ttm_data, "industry")

  taxonomy <- equities_taxonomy()

  result <- ttm_data %>%
    dplyr::left_join(
      taxonomy %>% dplyr::select(industry, subsector),
      by = "industry"
    )

  na_count <- sum(is.na(result$subsector))
  if (na_count > 0) {
    unmapped <- result %>%
      dplyr::filter(is.na(subsector)) %>%
      dplyr::distinct(industry) %>%
      dplyr::pull(industry)
    warning(
      na_count,
      " rows have NA subsector. Unmapped industries: ",
      paste(unmapped, collapse = ", ")
    )
  }

  result
}

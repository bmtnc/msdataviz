#' Join Equities Taxonomy to TTM Data
#'
#' Left joins subsector taxonomy onto TTM artifact by industry.
#' Converts sector, subsector, and industry to snake_case for consistent internal use.
#'
#' @param ttm_data TTM artifact tibble with `sector` and `industry` columns
#' @return TTM data with `subsector` column added; sector/subsector/industry in snake_case
#' @export
join_equities_taxonomy <- function(ttm_data) {
  avpipeline::validate_df_cols(ttm_data, c("sector", "industry"))

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

  result %>%
    dplyr::mutate(
      sector = to_snake_case(sector),
      subsector = to_snake_case(subsector),
      industry = to_snake_case(industry)
    )
}

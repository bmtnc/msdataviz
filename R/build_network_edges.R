#' Build Network Edges from Matched Data
#'
#' Transforms matched customer-supplier data into an edge list for graph construction.
#' Edges show money flow: payer -> payee.
#'
#' @param matched_data Data frame with ticker, counterparty_ticker, relationship columns
#' @return Data frame with from, to columns (distinct edges)
#' @keywords internal
build_network_edges <- function(matched_data) {
  matched_data %>%
    dplyr::filter(!is.na(counterparty_ticker)) %>%
    dplyr::mutate(
      from = dplyr::if_else(relationship == "customer", counterparty_ticker, ticker),
      to = dplyr::if_else(relationship == "customer", ticker, counterparty_ticker)
    ) %>%
    dplyr::select(from, to) %>%
    dplyr::distinct()
}

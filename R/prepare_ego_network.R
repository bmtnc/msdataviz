#' Prepare Ego Network
#'
#' Filters a network to the neighborhood around a specific ticker.
#'
#' @param edges Data frame with from, to, relationship columns
#' @param ego_ticker Ticker symbol to center the network on
#' @param depth Number of hops from ego to include (default: 2)
#' @return tidygraph object with nodes filtered to ego neighborhood,
#'   including degree, dist_from_ego, and is_ego attributes
#' @export
prepare_ego_network <- function(edges, ego_ticker, depth = 2) {
  # Build graph and calculate degree

g <- tidygraph::as_tbl_graph(edges, directed = TRUE) %>%
    tidygraph::mutate(degree = tidygraph::centrality_degree(mode = "all"))

  # Check if ego exists in network
  node_names <- igraph::V(g)$name
  if (!ego_ticker %in% node_names) {
    stop("Ticker '", ego_ticker, "' not found in network")
  }

  # Find ego's neighborhood
  ego_idx <- which(node_names == ego_ticker)
  neighbors <- igraph::ego(g, order = depth, nodes = ego_idx, mode = "all")
  neighbor_names <- node_names[unlist(neighbors)]

  # Calculate distances from ego
  distances <- igraph::distances(g, v = ego_idx, mode = "all")[1, ]

  # Filter to neighborhood and add distance attribute
  g_filtered <- g %>%
    tidygraph::filter(name %in% neighbor_names) %>%
    tidygraph::mutate(dist_from_ego = distances[match(name, names(distances))])

  # Add edge distance (max of endpoint distances)
  g_filtered <- g_filtered %>%
    tidygraph::activate(edges) %>%
    tidygraph::mutate(
      edge_dist = pmax(
        tidygraph::.N()$dist_from_ego[from],
        tidygraph::.N()$dist_from_ego[to]
      )
    ) %>%
    tidygraph::activate(nodes)

  # Mark ego node
  g_filtered <- g_filtered %>%
    tidygraph::mutate(is_ego = name == ego_ticker)

  g_filtered
}

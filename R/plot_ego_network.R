#' Plot Ego Network
#'
#' Creates a network visualization centered on a specific ticker.
#'
#' @param graph tidygraph object from prepare_ego_network()
#' @param ego_ticker Ticker symbol at center of network
#' @param depth Depth used for filtering (for subtitle)
#' @param seed Random seed for layout reproducibility (default: 42)
#' @return ggplot object
#' @keywords internal
plot_ego_network <- function(graph, ego_ticker, depth = 2, seed = 42) {
  n_nodes <- igraph::vcount(graph)
  n_edges <- igraph::ecount(graph)

  # Get node and edge data for statistics

  node_data <- graph %>%
    tidygraph::as_tibble()

  edge_data <- graph %>%
    tidygraph::activate(edges) %>%
    tidygraph::as_tibble()

  # Find ego index

  ego_idx <- which(node_data$name == ego_ticker)

  # Count direct linkages (1st degree)
  # Edges TO ego = customers (they pay ego)
  n_customers <- sum(edge_data$to == ego_idx)
  # Edges FROM ego = suppliers (ego pays them)
  n_suppliers <- sum(edge_data$from == ego_idx)

  # Count indirect linkages (2nd degree nodes)
  n_indirect <- sum(node_data$dist_from_ego == 2)

  set.seed(seed)

  ggraph::ggraph(graph, layout = "fr") +
    ggraph::geom_edge_arc(
      ggplot2::aes(
        edge_width = edge_dist,
        edge_alpha = edge_dist
      ),
      color = "gray40",
      arrow = ggplot2::arrow(length = ggplot2::unit(2, "mm"), type = "closed"),
      end_cap = ggraph::circle(3, "mm"),
      strength = 0.1
    ) +
    ggraph::geom_node_point(
      ggplot2::aes(
        size = degree,
        color = factor(dist_from_ego),
        alpha = factor(dist_from_ego)
      )
    ) +
    ggraph::geom_node_text(
      ggplot2::aes(label = name),
      repel = TRUE,
      size = 2.5,
      max.overlaps = 25
    ) +
    ggraph::scale_edge_width_continuous(range = c(0.8, 0.3), guide = "none") +
    ggraph::scale_edge_alpha_continuous(range = c(0.8, 0.25), guide = "none") +
    ggplot2::scale_color_manual(
      values = c("0" = "#FE7F2D", "1" = "#17255A", "2" = "#2E86AB"),
      guide = "none"
    ) +
    ggplot2::scale_alpha_manual(
      values = c("0" = 1, "1" = 0.8, "2" = 0.8),
      guide = "none"
    ) +
    ggplot2::scale_size_continuous(range = c(2, 12), guide = "none") +
    ggplot2::labs(
      title = paste0("Network Around ", ego_ticker, " (", depth, "-hop neighborhood)"),
      subtitle = paste0(
        "Arrows show money flow (payer \u2192 payee)\n",
        "Primary: ", n_customers, " customers, ", n_suppliers, " suppliers | ",
        "Indirect: ", n_indirect, " companies"
      ),
      caption = "Bubble size corresponds to number of connections"
    ) +
    ggraph::theme_graph() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0, size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 10),
      plot.caption = ggplot2::element_text(hjust = 0, size = 9)
    )
}

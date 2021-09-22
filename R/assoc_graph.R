#' @title Association graphs
#' @description Draws the association graphs based on given thresholds of joint entropy values.
#' @param dat Dataframe with rows as observations and columns as variables. Variables must all be observed or transformed categorical with finite range spaces.
#' @param cutoff The cutoff point for the edges to be drawn based on joint entropies. Default is 0 and draws all edges.
#' @return A ggraph object.
#' @details  To be completed
#' @author Termeh Shafie
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data. *Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique*, 129(1), 45-63.
#' @examples
#' @export
assoc_graph <- function(dat, cutoff = 0) {
  J <- joint_entropy(dat)
  adj <- J$matrix
  diag(adj) <- 0

  plot_title <- paste("J", ">",  cutoff, sep = " ")
  adj[adj < cutoff] <-  0
  ag <-
    igraph::graph_from_adjacency_matrix(adj, mode = 'undirected', weighted = TRUE)
  ag.p <- ggraph::ggraph(ag, layout = 'stress') +
    geom_edge_link0(edge_colour = "grey40", edge_width = E(ag)$weight) +
    geom_node_point(
      shape = 21,
      size = 20,
      fill = 'white',
      stroke = 1
    ) +
    geom_node_text(aes(label = V(ag)$name), size = 3.5) +
    ggtitle(plot_title) +
    theme_graph() +
    theme(legend.position = 'none') +
    coord_cartesian(clip = 'off')

  return(ag.p)
}

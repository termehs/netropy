#' @title Association Graphs
#' @description Draws association graphs/graphical models based on joint entropy values
#' to detect and visualize different dependence structures among the variables in the dataframe.
#' @param dat Dataframe with rows as observations and columns as variables. Variables must all be observed or transformed categorical with finite range spaces.
#' @param cutoff The cutoff point for the edges to be drawn based on joint entropies. Default is 0 and draws all edges.
#' @return A ggraph object.
#' @details Draws association graphs based on given thresholds of joint entropy values
#' between pairs of variables represented as nodes. Thickness of edges between pairs of nodes/variables
#' indicates strength of dependence between variables. Isolated nodes are completely
#' independent and paths through certain nodes/variables indicate conditional dependencies.
#' @author Termeh Shafie
#' @seealso \code{\link{joint_entropy}}
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data.
#' \emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique}, 129(1), 45-63.
#' \cr
#'
#' Nowicki, K., Shafie, T., & Frank, O. (Forthcoming 2022). \emph{Statistical Entropy Analysis of Network Data}.
#' @examples
#' library(ggraph)
#' # use internal data set and the attribute dataframe with 71 observations
#' data(lawdata)
#' df.att <- lawdata[[4]]
#' remove variable 'senior' as it is redundant
#' redundancy(df.att)
#' df.att <- df.att[,-1]
#' # association graph based on cutoff 0.3
#' assoc_graph(df.att, 0.3)
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
    geom_edge_link0(edge_colour = "grey40", edge_width = igraph::E(ag)$weight) +
    geom_node_point(
      shape = 21,
      size = 20,
      fill = 'white',
      stroke = 1
    ) +
    geom_node_text(aes(label = igraph::V(ag)$name), size = 3.5) +
    ggtitle(plot_title) +
    theme_graph() +
    theme(legend.position = 'none') +
    coord_cartesian(clip = 'off')

  return(ag.p)
}

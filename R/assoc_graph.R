#' @title Association Graphs
#' @description Draws association graphs (graphical models) based on joint entropy values
#' to detect and visualize different dependence structures among the variables in the dataframe.
#' @param dat dataframe with rows as observations and columns as variables. Variables must all be observed or transformed categorical with finite range spaces.
#' @param cutoff the cutoff point for the edges to be drawn based on joint entropies. Default is 0 and draws all edges.
#' @return A ggraph object with nodes representing all variables in \code{dat} and edges
#' representing (the strength of) associations between them based on joint entropies.
#' @details Draws association graphs based on given thresholds of joint entropy values
#' between pairs of variables represented as nodes. Thickness of edges between pairs of nodes/variables
#' indicates the strength of dependence between them. Isolated nodes are completely
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
#' # use internal data set
#' data(lawdata)
#' df.att <- lawdata[[4]]
#'
#' # three steps of data editing:
#' # 1. categorize variables 'years' and 'age' based on
#' # approximately three equally size groups (values based on cdf)
#' # 2. make sure all outcomes start from the value 0 (optional)
#' # 3. remove variable 'senior' as it consists of only unique values (thus redundant)
#' df.att.ed <- data.frame(
#'    status   = df.att$status,
#'    gender   = df.att$gender,
#'    office   = df.att$office-1,
#'    years    = ifelse(df.att$years<=3,0,
#'               ifelse(df.att$years<=13,1,2)),
#'    age      = ifelse(df.att$age<=35,0,
#'                 ifelse(df.att$age<=45,1,2)),
#'    practice = df.att$practice,
#'    lawschool= df.att$lawschool-1)
#'
#' # association graph based on cutoff 0.15
#' assoc_graph(df.att.ed, 0.15)
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
    ggraph::geom_edge_link0(edge_colour = "grey40", edge_width = igraph::E(ag)$weight) +
    ggraph::geom_node_point(
      shape = 21,
      size = 20,
      fill = 'white',
      stroke = 1
    ) +
    ggraph::geom_node_text(ggplot2::aes(label = igraph::V(ag)$name), size = 3.5) +
    ggplot2::ggtitle(plot_title) +
    ggraph::theme_graph(base_family = "") +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::coord_cartesian(clip = 'off')

  return(ag.p)
}

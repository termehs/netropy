#' @title Get Dyad Variables
#' @description Transforms vertex variables or observed directed/undirected ties
#' into dyad variables.
#' @param var variable vector (actor attribute) or adjacency matrix (ties)
#' to be transformed to a dyad variable.
#' @param type either 'att' for actor attribute (default) or 'tie' for relations.
#' @return Data frame with three columns:
#' first two columns show the vertex pairs \code{u} and \code{v} where \code{u<v} ,
#' and the third column gives the transformed dyadic variable \code{var}.
#' @details For actor attributes, unique pairs of original attribute values
#' constitute the outcome space. Note that the actor  attributes need
#' to be categorical and on ordinal scale with finite range spaces.
#'
#' For directed relations, pairs of indicators from the adjacency matrix constitute
#' the four outcomes representing possible combinations of sending and receiving ties:
#' (0,0), (0,1), (1,0), (1,1).
#'
#' For undirected relations, an indicator variable which is directly read from the
#' adjacency matrix represents the dyadic variable.
#'
#' Warning message is shown if actor attribute has too many unique outcomes
#' as it will yield too many possible outcomes when converted in to a dyad variable.
#'
#' To be completed.
#' @author Termeh Shafie
#' @seealso
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data. *Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique*, 129(1), 45-63.
#' @examples To be added
#' @export

get_dyad_var <- function(var, type = 'att') {
  if (type == 'att') {
    n <- length(var)
    if (length(unique(var))>=7) {
      warning('consider categorizing variable as it yields too many dyadic outcomes')
    }
    var.matching <- outer(c(min(var):max(var)),c(min(var):max(var)),paste)
    dyad <- expand.grid(u=1:n,v=1:n)
    dyad$var <- match(paste(var[dyad$u],var[dyad$v]),var.matching)-1
    dyad <- dyad[dyad$u<dyad$v,]
  }
  else if (type == 'tie') {
    n <- dim(var)[1]
    if (isSymmetric(var) == TRUE) {
      dyad <- data.frame(
        u=rep(1:n,n),v=rep(1:n,each=n),
        dyad.var = c(var)
      )
      dyad <- dyad[dyad$u<dyad$v,]
      message('two outcomes based on an indicator variable for the undirected relation is created')
    }
    else if (isSymmetric(var) == FALSE) {
      var.matching <- outer(c(0,1),c(0,1),paste)
      dyad <- data.frame(u=rep(1:n,n), v=rep(1:n,each=n),
                         var=paste(c(t(var)),c(var)))
      dyad$var <- match(dyad$var,var.matching)-1
      dyad  <- dyad[dyad$u<dyad$v,]
      message('four outcomes based on pairs of indicators for the directed relation is created')
    }
  }

  return(dyad)
}





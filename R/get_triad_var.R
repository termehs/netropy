#' @title Get Triad Variables
#' @description Transforms vertex variables or observed directed/undirected ties
#' into triad variables.
#' @param var variable vector (actor attribute) or adjacency matrix (ties)
#' to be transformed to a triad variable.
#' @param type either 'att' for actor attribute (default) or 'tie' for relations.
#' @return Dataframe with four columns:
#' first three columns show the vertex triad \code{u}, \code{v}, \code{w} ,
#' and the fourth column gives the value of the transformed triadic variable \code{var}.
#' @details For actor attributes, unique triples of original attribute values
#' constitute the outcome space. Note that the actor
#' attributes need to be categorical with finite range spaces.
#' For example, binary attributes have 8 possible triadic outcomes
#' (0,0,0),(1,0,0),(0,1,0),(1,1,0),(0,0,1),(1,0,1),(0,1,1),(1,1,1)
#' which are coded 0-7.
#' Warning message is shown if actor attribute has too many unique outcomes
#' as it will yield too many possible outcomes once converted in to a triad variable.
#' \cr
#'
#' For directed relations, a sequence of indicators of length 6 created from the adjacency matrix
#' constitutes the 64 outcomes representing possible combinations of sending and receiving ties.
#' \cr
#'
#' For undirected relations, triples of indicators are created from the adjacency matrix.
#' \cr
#'
#'
#' @author Termeh Shafie
#' @seealso \code{\link{get_dyad_var}}
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data.
#' \emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique}, 129(1), 45-63.
#' \cr
#'
#' Nowicki, K., Shafie, T., & Frank, O. (Forthcoming 2022). \emph{Statistical Entropy Analysis of Network Data}.
#' @examples
#' # use internal data set
#' data(lawdata)
#' adj.advice <- lawdata[[1]]
#' adj.cowork <-lawdata[[3]]
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
#' # actor attribute converted to triad variable
#' triad.gend <- get_triad_var(df.att.ed$gender, 'att')
#'
#' # directed tie converted to triad variable
#' triad.adv <- get_triad_var(adj.advice, type = 'tie')
#'
#' # undirected tie converted to triad variable
#' triad.cwk <- get_triad_var(adj.cowork, type = 'tie')
#' @export

get_triad_var <- function(var, type = 'att') {
  if (type == 'att') {
    n <- length(var)
    triads <- t(utils::combn(1:n,3))
    triads <- cbind(triads,var[triads[,1]], var[triads[,2]],var[triads[,3]])
    triad.var <- data.frame(u=triads[,1], v=triads[,2], w=triads[,3],
                            u.var=triads[,4], v.var=triads[,5], w.var=triads[,6])
    triad.var$uvw <- paste(triad.var$u.var,
                           triad.var$v.var,
                           triad.var$w.var)
    var.matching <- apply(expand.grid(min(var):max(var),
                                      min(var):max(var),
                                      min(var):max(var)),1, paste,collapse=" ")
    triad.var$var <- match(triad.var$uvw,var.matching)-1
    if (length(unique(var))>=7) {
      warning('consider categorizing variable as it yields too many dyadic outcomes')
    }
  }
  else if (type == 'tie') {
    n <- dim(var)[1]
    if (isSymmetric(var) == TRUE) {
      triads <- t(utils::combn(1:n,3))
      triads <- cbind(triads,
                      var[triads[,c(1,2)]],
                      var[triads[,c(1,3)]],
                      var[triads[,c(2,3)]]
      )
      colnames(triads) <- c("u","v","w",
                            "uv","uw","vw")
      triad.var <- as.data.frame(triads)
      triad.var$uv.uw.vw <- apply(triad.var[,4:6],1,
                                  paste,collapse=" ")
      grid <- expand.grid(0:1,0:1,0:1)
      var.matching <- apply(grid,1,
                            function(x) paste(x,collapse=" "))
      triad.var$var <- match(triad.var$uv.uw.vw,var.matching)-1
      message('8 outcomes based on triples of indicators for the undirected relation are created')
    }
    else if (isSymmetric(var) == FALSE) {
      triads <- t(utils::combn(1:n,3))
      triads <- cbind(triads, var[triads[,c(1,2)]],
                              var[triads[,c(2,1)]],
                              var[triads[,c(1,3)]],
                              var[triads[,c(3,1)]],
                              var[triads[,c(2,3)]],
                              var[triads[,c(3,2)]])
      colnames(triads) <- c("u","v","w",
                            "uv","vu","uw","wu","vw","wv")
      triad.var <- as.data.frame(triads)
      triad.var$seq <- apply(triad.var[,4:9],1,
                             paste,collapse=" ")
      grid <- expand.grid(0:1,0:1,0:1,0:1,0:1,0:1)
      var.matching <- apply(grid,1,
                            function(x) paste(x,collapse=" "))
      triad.var$var <- match(triad.var$seq,var.matching)-1
      message('64 outcomes based on a sequence of 6 indicators for the directed relation are created')
    }
  }

  out.var <- as.data.frame(cbind(triad.var$u, triad.var$v, triad.var$w, triad.var$var))
  names(out.var) <- c("u","v","w","var")
  return(out.var)
}

#' @title Get Dyad Variables
#' @description Transforms vertex variables or observed directed/undirected ties
#' into dyad variables.
#' @param var variable vector (actor attribute) or adjacency matrix (ties)
#' to be transformed to a dyad variable.
#' @param type either 'att' for actor attribute (default) or 'tie' for relations.
#' @return Dataframe with three columns:
#' first two columns show the vertex pairs \code{u} and \code{v} where \code{u<v} ,
#' and the third column gives the value of the transformed dyadic variable \code{var}.
#' @details Dyad variables are given as pairs of incident vertex variables
#' or actor attributes. Here, unique pairs of original attribute values
#' constitute the outcome space. Note that the actor  attributes need
#' to be categorical with finite range spaces. For example, binary
#' attribute yields outcome space (0,0), (0,1), (1,0), (1,1) coded as (0),(1),(2),(3).
#' Warning message is shown if actor attribute has too many unique outcomes
#' as it will yield too many possible outcomes once converted in to a dyad variable.
#' \cr
#'
#' For directed relations, pairs of indicators from the adjacency matrix constitute
#' the four outcomes representing possible combinations of sending and receiving ties:
#' (0,0), (0,1), (1,0), (1,1) coded as (0),(1),(2),(3).
#' \cr
#'
#' For undirected relations, an indicator variable which is directly read from the
#' adjacency matrix represents the dyadic variable.
#' \cr
#'
#' @author Termeh Shafie
#' @seealso \code{\link{get_triad_var}}
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data.
#' \emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique}, 129(1), 45-63.
#' @examples
#' # use internal data set
#' data(lawdata)
#' adj.advice <- lawdata[[1]]
#' adj.cowork <- lawdata[[3]]
#' df.att <- lawdata[[4]]
#'
#' # three steps of data editing of attribute dataframe:
#' # 1. categorize variables 'years' and 'age' based on
#' # approximately three equally size groups (values based on cdf)
#' # 2. make sure all outcomes start from the value 0 (optional)
#' # 3. remove variable 'senior' as it consists of only unique values (thus redundant)
#' df.att.ed <- data.frame(
#'     status = df.att$status,
#'     gender = df.att$gender,
#'     office = df.att$office - 1,
#'     years = ifelse(df.att$years <= 3, 0,
#'         ifelse(df.att$years <= 13, 1, 2)
#'     ),
#'     age = ifelse(df.att$age <= 35, 0,
#'         ifelse(df.att$age <= 45, 1, 2)
#'     ),
#'     practice = df.att$practice,
#'     lawschool = df.att$lawschool - 1
#' )
#'
#' # actor attribute converted to dyad variable
#' dyad.gend <- get_dyad_var(df.att.ed$gender, "att")
#'
#' # directed tie converted to dyad variable
#' dyad.adv <- get_dyad_var(adj.advice, "tie")
#'
#' # undirected tie converted to dyad variable
#' dyad.cwk <- get_dyad_var(adj.cowork, "tie")
#' @export

get_dyad_var <- function(var, type = "att") {
    if (type == "att") {
        n <- length(var)
        if (length(unique(var)) >= 7) {
            warning("consider categorizing variable as it yields too many dyadic outcomes")
        }
        var.matching <- outer(c(min(var):max(var)), c(min(var):max(var)), paste)
        dyad <- expand.grid(u = 1:n, v = 1:n)
        dyad$var <- match(paste(var[dyad$u], var[dyad$v]), var.matching) - 1
        dyad <- dyad[dyad$u < dyad$v, ]
    } else if (type == "tie") {
        n <- dim(var)[1]
        if (isSymmetric(var) == TRUE) {
            dyad <- data.frame(
                u = rep(1:n, n), v = rep(1:n, each = n),
                var = c(var)
            )
            dyad <- dyad[dyad$u < dyad$v, ]
            message("two outcomes based on an indicator variable for the undirected relation is created")
        } else if (isSymmetric(var) == FALSE) {
            var.matching <- outer(c(0, 1), c(0, 1), paste)
            dyad <- data.frame(
                u = rep(1:n, n), v = rep(1:n, each = n),
                var = paste(c(t(var)), c(var))
            )
            dyad$var <- match(dyad$var, var.matching) - 1
            dyad <- dyad[dyad$u < dyad$v, ]
            message("four outcomes based on pairs of indicators for the directed relation is created")
        }
    }

    return(dyad)
}

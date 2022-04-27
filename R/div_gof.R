#' @title Divergence Tests of Goodness of Fit
#' @description Tests of various hypothetical structural models \emph{p0} against
#' the general model \emph{p} which is estimated using empirical data.
#' @param dat dataframe with rows as observations and columns as variables.
#' Variables must all be observed or transformed categorical with finite range spaces.
#' @param var1 vector of variables in \code{dat} specified to be independent to
#' those specified in \code{var2} under \emph{p0}
#' @param var2 vector of variables in \code{dat} specified to be independent to
#' those specified in \code{var1}
#' @param var_cond vector of variables in \code{dat} to condition the independence specification on,
#' must be different variables than those specified in \code{var1} and \code{var2}).
#' Default empty (no conditioning).
#' @return Test results
#' @details description of tests
#'
#' @author Termeh Shafie
#' @seealso \code{\link{get_dyad_var}}
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data.
#' \emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique}, 129(1), 45-63.
#' \cr
#'
#' Nowicki, K., Shafie, T., & Frank, O. (Forthcoming 2022). \emph{Statistical Entropy Analysis of Network Data}.
#' @examples
#'
div_gof <- function(dat, var1, var2, var_cond = 0) {
  idx_var1 <- which(names(dat) == var1)
  idx_var2 <- which(names(dat) == var2)

  varname.orig <- colnames(dat)
  varname.new <- sprintf("V%d", 1:ncol(dat))
  names(dat) <- varname.new

  # pairwise independence
  if (length(idx_var1) == 1 & length(idx_var2) == 1 & var_cond == 0) {
      J <- joint_entropy(dat, dec = 3)
      J <- J$matrix
      J <- J[lower.tri(J)] = t(J)[lower.tri(J)]
      D <- 2*dim(dat)[1]*(J[idx_var1,idx_var2])
      chi2 <- (2*dim(dat)[1]*D)/(log2(exp(1)))

    # degrees of freedom for test statistic chi2
      df_var1 = length(range(dat[,idx_var1]))
      df_var2 = length(range(dat[,idx_var2]))

      df_chi2 = (df_var1-1)*(df_var2-1)

    # critical value
      cv = df_chi2 + sqrt((8*df_chi2))






      # alternatively
      H <- entropy_bivar(dat)
      D <- 2*dim(dat)[1]*(H[idx_set1,idx_set1]
                          + H[idx_set2,idx_set2]
                          - H[idx_set1,idx_set2])
    }


  # empirical distribution/general model, is this needed?
   tab <- table(dat[,idx_set1:idx_set2])
   p <- addmargins(prop.table(tab))

   # independent model, is this needed?
   q <- p["Sum",]*p[,"Sum"]

   # chi.stat = 2nD(pq) where D = H_X + H_Y − H_(X,Y) = 2n J(X,Y) pairwise
   # conditional specifications
   # nested specifications



}

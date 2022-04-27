#' @title Divergence Tests of Goodness of Fit
#' @description Tests of various hypothetical structural models \emph{p0} against
#' the general model \emph{p} which is estimated using empirical data.
#' @param dat dataframe with rows as observations and columns as variables.
#' Variables must all be observed or transformed categorical with finite range spaces.
#' @param var1 vector of variables in \code{dat} to be tested for independence against
#' those specified in \code{set2}
#' @param var2 vector of variables in \code{dat} to be tested for independence against
#' those specified in \code{set1}
#' @param var_cond vector of variables in \code{dat} to condition the independence test on,
#' must be different variables than those specified in \code{set1} and \code{set2}).
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

  idx_set1 <- which(names(dat) == var1)
  idx_set2 <- which(names(dat) == var2)

  # pairwise independence
  if (length(idx_set1 == 1 & length(set2) == 1 & var_cond == 0) {
    varname.orig <- colnames(dat)
    varname.new <- sprintf("V%d", 1:ncol(dat))
    names(dat) <- varname.new

      J <- joint_entropy(dat, dec = 3)
      J <- J$matrix
      D <- 2*dim(dat)[1]*(J[idx_set1,idx_set2])
      chi2 <- 2*dim(dat)[1]/(log2(exp(1)))

    # degrees of freedom for test statistic chi2
      df_set1 = length(range(dat[,idx_set1]))
      df_set2 = length(range(dat[,idx_set2]))

      df_chi2 = (df_set1-1)*(df_set2-1)

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

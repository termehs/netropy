#' @title Divergence Tests of Goodness of Fit
#' @description Tests of various hypothetical structural models \emph{p0} against
#' the general model \emph{p} which is estimated using empirical data.
#' @param dat dataframe with rows as observations and columns as variables.
#' Variables must all be observed or transformed categorical with finite range spaces.
#' @param var1 variable in \code{dat} specified to be independent to
#' those specified in \code{var2} under \emph{p0}
#' @param var2 variable in \code{dat} specified to be independent to
#' those specified in \code{var1}
#' @param var_cond variable in \code{dat} to condition the independence specification on,
#' must be different variables than those specified in \code{var1} and \code{var2}).
#' Default empty (no conditioning).
#' @return Message indicating whether the hypothesis with the specified independence model
#' can be rejected or not on approximately 5% level of significance.
#' \item{summary}{Data frame including the value of the Divergence and its degrees of freedom}
#' @details this function is currently implemented to only test the goodness of fit of models specified as
#' \emph{X} independent of \emph{Y}, or \emph{X} independent of \emph{Y} given \emph{Z}.
#' @author Termeh Shafie
#' @seealso \code{\link{joint_entropy}},   \code{\link{assoc_graph}},  \code{\link{entropy_trivar}}
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data.
#' \emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique}, 129(1), 45-63.
#' \cr
#'
#' Nowicki, K., Shafie, T., & Frank, O. (Forthcoming 2022). \emph{Statistical Entropy Analysis of Network Data}.
#' @examples
#'
div_gof <- function(dat, var1, var2, var_cond = NULL) {
  idx_var1 <- which(names(dat) == var1)
  idx_var2 <- which(names(dat) == var2)

  if (is.null(var_cond) == FALSE) {
  idx_cond <- which(names(dat) == var_cond)
  }

  varname.orig <- colnames(dat)
  varname.new <- sprintf("V%d", 1:ncol(dat))
  names(dat) <- varname.new

  # pairwise independence
  if (length(idx_var1) == 1 & length(idx_var2) == 1 & is.null(var_cond)) {
      J <- joint_entropy(dat, dec = 3)
      J <- J$matrix
      J[lower.tri(J)] = t(J)[lower.tri(J)]
      D <- 2*dim(dat)[1]*(J[idx_var1,idx_var2])
      chi2 <- (2*dim(dat)[1]*D)/(log2(exp(1)))

    # degrees of freedom for test statistic chi2
      df_var1 = length(unique(dat[,idx_var1]))
      df_var2 = length(unique(dat[,idx_var2]))

      df_chi2 = (df_var1-1)*(df_var2-1)

    # critical value at 5% level
      cv = df_chi2 + sqrt((8*df_chi2))
        if (chi2 > cv) {
          return(message("the specified model of independence cannot be rejected"))
        }
        else if (chi2 <= cv) {
          return(message("the specified model of independence is rejected"))
        }
  }


  # conditional independence
  else if (length(idx_var1) == 1 & length(idx_var2) == 1 & length(idx_cond) == 1) {
    H <- entropy_bivar(dat)
    H[lower.tri(H)] = t(H)[lower.tri(H)]
    D <- 2*dim(dat)[1]*(H[idx_var1,idx_cond ] +
                          H[idx_var2, idx_cond ] -
                          H[idx_cond, idx_cond ] -
                          H[idx_var1, idx_var2])
    chi2 <- (2*dim(dat)[1]*D)/(log2(exp(1)))

    # degrees of freedom for test statistic chi2
    df_var1 = length(unique(dat[,idx_var1]))
    df_var2 = length(unique(dat[,idx_var2]))
    df_varcond = length(unique(dat[,idx_cond ]))

    df_chi2 = (df_var1-1)*(df_var2-1)*df_varcond

    # critical value at 5% level
    cv = df_chi2 + sqrt((8*df_chi2))
    if (chi2 > cv) {
      return(message("the specified model of independence cannot be rejected"))
    }
    else if (chi2 <= cv) {
      return(message("the specified model of independence is rejected"))
    }
  }


  # warning message for wrongful model specification
  else {
    warning("function currently implemented for tests of the kind X is indepndent of Y: Z _|_Y,
            and X is independent of Y given Z: X_|_Y|Z ")
  }
}








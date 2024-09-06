#' @title Divergence Tests of Goodness of Fit
#' @description Tests of various hypothetical structural models \emph{p0} against
#' the general model \emph{p} which is estimated using empirical data.
#' @param dat dataframe with rows as observations and columns as variables.
#' Variables must all be observed or transformed categorical with finite range spaces.
#' @param var1 variable name as character in \code{dat} specified to be independent to
#' those specified in \code{var2} under \emph{p0}
#' @param var2 variable name as character in \code{dat} specified to be independent to
#' those specified in \code{var1}
#' @param var_cond character of variable name in \code{dat} to condition the independence specification on,
#' must be different variables than those specified in \code{var1} and \code{var2}).
#' Default = empty (no conditioning).
#' @return Message indicating whether the hypothesis with the specified independence model
#' can be rejected or not on approximately 5\% level of significance.
#' \item{summary}{Dataframe including the value of the divergence \emph{D} and its degrees of freedom}
#' @details this function is currently implemented to only test goodness of fit of models specified as
#' \emph{X} independent of \emph{Y}, or \emph{X} independent of \emph{Y} given \emph{Z}.
#' @author Termeh Shafie
#' @seealso \code{\link{joint_entropy}},   \code{\link{assoc_graph}},  \code{\link{entropy_trivar}}
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data.
#' \emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique}, 129(1), 45-63.
#' @examples
#' # Data editing and creation of dyad variables:
#' data(lawdata)
#' adj.advice <- lawdata[[1]]
#' adj.friend <- lawdata[[2]]
#' adj.cowork <-lawdata[[3]]
#' df.att <- lawdata[[4]]
#' att.var <-
#' data.frame(
#'  status   = df.att$status-1,
#'  gender   = df.att$gender,
#'  office   = df.att$office-1,
#'  years    = ifelse(df.att$years<=3,0,
#'             ifelse(df.att$years<=13,1,2)),
#'  age  = ifelse(df.att$age<=35,0,
#'         ifelse(df.att$age<=45,1,2)),
#'  practice = df.att$practice,
#'  lawschool= df.att$lawschool-1)
#' dyad.status    <- get_dyad_var(att.var$status, type = 'att')
#' dyad.gender    <- get_dyad_var(att.var$gender, type = 'att')
#' dyad.office    <- get_dyad_var(att.var$office, type = 'att')
#' dyad.years     <- get_dyad_var(att.var$years, type = 'att')
#' dyad.age       <- get_dyad_var(att.var$age, type = 'att')
#' dyad.practice  <- get_dyad_var(att.var$practice, type = 'att')
#' dyad.lawschool <- get_dyad_var(att.var$lawschool, type = 'att')
#' dyad.cwk    <- get_dyad_var(adj.cowork, type = 'tie')
#' dyad.adv    <- get_dyad_var(adj.advice, type = 'tie')
#' dyad.frn    <- get_dyad_var(adj.friend, type = 'tie')
#' dyad.var <-
#' data.frame(cbind(status    = dyad.status$var,
#'                  gender    = dyad.gender$var,
#'                  office    = dyad.office$var,
#'                  years     = dyad.years$var,
#'                  age       = dyad.age$var,
#'                  practice  = dyad.practice$var,
#'                  lawschool = dyad.lawschool$var,
#'                  cowork    = dyad.cwk$var,
#'                  advice    = dyad.adv$var,
#'                  friend    = dyad.frn$var)
#'                  )
#' # To test whether friend is independent of cowork given advice:
#' div_gof(dat = dyad.var, var1 = "friend", var2 = "cowork", var_cond = "advice")
#' @export

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
      D <- (J[idx_var1,idx_var2])
      chi2 <- (2*dim(dat)[1]*D)/(log2(exp(1)))

    # degrees of freedom for test statistic chi2
      df_var1 = length(unique(dat[,idx_var1]))
      df_var2 = length(unique(dat[,idx_var2]))

      df_chi2 = (df_var1-1)*(df_var2-1)

    # critical value at 5% level
      cv = df_chi2 + sqrt((8*df_chi2))
        if (chi2 > cv) {
          message("the specified model of independence cannot be rejected")
        }
        else if (chi2 <= cv) {
          message("the specified model of independence is rejected")
        }
  }

  # conditional independence
  else if (length(idx_var1) == 1 & length(idx_var2) == 1 & length(idx_cond) == 1) {
    H <- entropy_bivar(dat)
    H[lower.tri(H)] = t(H)[lower.tri(H)]
    D <- (H[idx_var1,idx_cond ] +
                          H[idx_var2, idx_cond ] -
                          H[idx_cond, idx_cond ] -
                          H[idx_var1, idx_var2])
    chi2 <- (2*dim(dat)[1]*D)/(log2(exp(1)))

    # degrees of freedom for test statistic chi2
    df_var1 = length(unique(dat[,idx_var1]))
    df_var2 = length(unique(dat[,idx_var2]))
    df_varcond = length(unique(dat[,idx_cond ]))

    df_chi2 = (df_var1-1)*(df_var2-1)*df_varcond

    # critical value at approximately 5% level
    cv = df_chi2 + sqrt((8*df_chi2))
    if (chi2 > cv) {
      message("the specified model of conditional independence cannot be rejected")
    }
    else if (chi2 <= cv) {
      message("the specified model of conditional independence is rejected")
    }
}


  # warning message for wrongful model specification
  else {
    warning("function currently only implemented for tests of the kind X is independent of Y: Z _|_Y,
            and X is independent of Y given Z: X_|_Y|Z ")
  }

    # output: test summary
    # p <- round(1 - pchisq(chi2, df_chi2),3)
    summary <- as.data.frame(cbind(round(D,2), df_chi2))
    rownames(summary)<- c()
    colnames(summary) <- c('D', 'df(D)')
    return(summary)
}








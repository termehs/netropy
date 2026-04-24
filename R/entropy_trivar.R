#' @title Trivariate Entropy
#' @description Computes trivariate entropies of all triples of discrete
#' variables in a multivariate data set.
#' @param dat dataframe with rows as observations and columns as variables.
#' Variables must all be observed or transformed categorical with finite range spaces.
#' @return Dataframe with the first three columns representing possible triples
#' of variables (\code{X}, \code{Y}, \code{Z}) and the fourth column giving
#' trivariate entropies \code{H(X,Y,Z)}.
#' @details Trivariate entropies can be used to check for functional relationships
#' and stochastic independence between triples of variables.
#'
#' The trivariate entropy \emph{H(X,Y,Z)} of three discrete random variables
#' \emph{X}, \emph{Y}, and \emph{Z} is bounded according to
#' \deqn{H(X,Y) <= H(X,Y,Z) <= H(X,Z) + H(Y,Z) - H(Z).}
#'
#' The increment between the trivariate entropy and its lower bound is equal to
#' the expected conditional entropy.
#' @author Termeh Shafie
#' @seealso \code{\link{entropy_bivar}}, \code{\link{prediction_power}}
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data.
#' \emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique}, 129(1), 45-63.
#' @examples
#' # use internal data set
#' data(lawdata)
#' df_att <- lawdata[[4]]
#'
#' # data editing:
#' # 1. categorize variables 'years' and 'age' into approximately
#' # equally sized groups
#' # 2. recode selected variables so categories start at 0
#' att_var <- data.frame(
#'   status    = df_att$status - 1,
#'   gender    = df_att$gender,
#'   office    = df_att$office - 1,
#'   years     = ifelse(df_att$years <= 3, 0,
#'                 ifelse(df_att$years <= 13, 1, 2)),
#'   age       = ifelse(df_att$age <= 35, 0,
#'                 ifelse(df_att$age <= 45, 1, 2)),
#'   practice  = df_att$practice,
#'   lawschool = df_att$lawschool - 1
#' )
#'
#' # calculate trivariate entropies
#' h_trivar <- entropy_trivar(att_var)
#' @export

entropy_trivar <- function(dat) {
  varname_orig <- colnames(dat)
  varname_new <- sprintf("V%d", seq_len(ncol(dat)))
  names(dat) <- varname_new

  p <- ncol(dat)
  n <- nrow(dat)
  log2_n <- log2(n)
  n_out <- choose(p, 3)

  X <- character(n_out)
  Y <- character(n_out)
  Z <- character(n_out)
  H <- numeric(n_out)

  k <- 0
  for (x in seq_len(p - 2)) {
    for (y in (x + 1):(p - 1)) {
      for (z in (y + 1):p) {
        k <- k + 1

        frq <- as.vector(table(dat[, x], dat[, y], dat[, z]))
        pos <- frq[frq > 0]
        h_tmp <- log2_n - sum(pos * log2(pos)) / n

        X[k] <- varname_orig[x]
        Y[k] <- varname_orig[y]
        Z[k] <- varname_orig[z]
        H[k] <- round(h_tmp, 3)
      }
    }
  }

  data.frame(
    X = X,
    Y = Y,
    Z = Z,
    `H(X,Y,Z)` = H,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

#' @title Tetravariate Entropy
#' @description Computes tetravariate entropies, expected conditional entropies,
#' and expected conditional joint entropies for all quadruples of variables in a
#' multivariate discrete data set.
#' @param dat dataframe with rows as observations and columns as variables.
#' Variables must be categorical with finite range spaces.
#' @param dec number of decimals used for rounding the entropy values.
#' Default is 2.
#' @return A dataframe with one row for each ordered decomposition of four
#' variables into predictors and conditioning variables. The columns are:
#' \item{X}{first variable in the pair of interest.}
#' \item{Y}{second variable in the pair of interest.}
#' \item{Z}{first conditioning variable.}
#' \item{U}{second conditioning variable.}
#' \item{H_XYZU}{tetravariate entropy \emph{H(X,Y,Z,U)}.}
#' \item{EH_U_XYZ}{expected conditional entropy \emph{EH(U|X,Y,Z)}.}
#' \item{EH_Z_XYU}{expected conditional entropy \emph{EH(Z|X,Y,U)}.}
#' \item{EJ_XY_ZU}{expected conditional joint entropy \emph{EJ(X,Y|Z,U)}.}
#' @details For four variables \emph{X}, \emph{Y}, \emph{Z}, and \emph{U}, the
#' tetravariate entropy is denoted \emph{H(X,Y,Z,U)}. The expected conditional
#' entropies are computed as
#' \deqn{EH(U|X,Y,Z) = H(X,Y,Z,U) - H(X,Y,Z)}
#' and
#' \deqn{EH(Z|X,Y,U) = H(X,Y,Z,U) - H(X,Y,U).}
#' The expected conditional joint entropy is computed as
#' \deqn{EJ(X,Y|Z,U) = H(X,Z,U) + H(Y,Z,U) - H(Z,U) - H(X,Y,Z,U).}
#' This quantity measures deviation from conditional independence of the form
#' \eqn{X \perp Y \,\vert\, Z, U}{X is independent of Y given Z, U}.
#' Smaller values indicate weaker conditional dependence.
#' @author Termeh Shafie
#' @seealso \code{\link{entropy_trivar}}, \code{\link{entropy_bivar}},
#' \code{\link{prediction_power}}
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of
#' network data. \emph{Bulletin of Sociological Methodology/Bulletin de
#' Méthodologie Sociologique}, 129(1), 45-63.
#' @examples
#' # use internal data set
#' data(lawdata)
#'
#' # extract node attributes
#' df_att <- lawdata[[4]]
#'
#' # data editing:
#' # 1. discretize 'years' and 'age' into three approximately balanced groups
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
#' # compute tetravariate entropy quantities for five selected variables
#' entropy_tetravar(
#'   dat = att_var[, c("gender", "years", "age", "office", "practice")]
#' )
#'
#' @export

entropy_tetravar <- function(dat, dec = 2) {
  entropy_emp <- function(x) {
    tab <- table(x)
    p <- as.vector(tab) / sum(tab)
    p <- p[p > 0]
    sum(p * log2(1 / p))
  }

  vars <- colnames(dat)
  n_out <- choose(length(vars), 4) * 6

  cache <- new.env(hash = TRUE, parent = emptyenv())
  H <- function(cols) {
    key <- paste(sort(cols), collapse = "\r")
    val <- cache[[key]]
    if (is.null(val)) {
      val <- entropy_emp(dat[cols])
      cache[[key]] <- val
    }
    val
  }

  X <- character(n_out)
  Y <- character(n_out)
  Z <- character(n_out)
  U <- character(n_out)
  H_XYZU <- numeric(n_out)
  EH_U_XYZ <- numeric(n_out)
  EH_Z_XYU <- numeric(n_out)
  EJ_XY_ZU <- numeric(n_out)

  k <- 1
  for (quad in utils::combn(vars, 4, simplify = FALSE)) {
    h_xyzu <- H(quad)
    xy_pairs <- utils::combn(quad, 2, simplify = FALSE)

    for (xy in xy_pairs) {
      zu <- setdiff(quad, xy)

      x <- xy[1]
      y <- xy[2]
      z <- zu[1]
      u <- zu[2]

      h_xyz <- H(c(x, y, z))
      h_xyu <- H(c(x, y, u))
      h_xzu <- H(c(x, z, u))
      h_yzu <- H(c(y, z, u))
      h_zu <- H(c(z, u))

      X[k] <- x
      Y[k] <- y
      Z[k] <- z
      U[k] <- u
      H_XYZU[k] <- h_xyzu
      EH_U_XYZ[k] <- h_xyzu - h_xyz
      EH_Z_XYU[k] <- h_xyzu - h_xyu
      EJ_XY_ZU[k] <- h_xzu + h_yzu - h_zu - h_xyzu

      k <- k + 1
    }
  }

  data.frame(
    X = X,
    Y = Y,
    Z = Z,
    U = U,
    H_XYZU = round(H_XYZU, dec),
    EH_U_XYZ = round(EH_U_XYZ, dec),
    EH_Z_XYU = round(EH_Z_XYU, dec),
    EJ_XY_ZU = round(EJ_XY_ZU, dec),
    stringsAsFactors = FALSE
  )
}

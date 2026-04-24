#' @title Divergence Tests of Goodness of Fit
#' @description Performs divergence-based goodness-of-fit tests for discrete data,
#' including tests of uniformity, pairwise independence, conditional independence,
#' and nested model comparisons.
#' @param dat dataframe with rows as observations and columns as variables.
#' Variables must be categorical with finite range spaces.
#' @param var_uniform character name of a variable in \code{dat} to test for uniformity.
#' @param var1 character name of the first variable.
#' @param var2 character name of the second variable.
#' @param var_cond optional character vector of conditioning variables.
#' @param model_full list containing \code{D} and \code{df} for the full model.
#' @param model_reduced list containing \code{D} and \code{df} for the reduced model.
#' @param alpha significance level. Default is 0.05.
#' @param dec number of decimals for rounding. Default is 3.
#' @param use_approx_cv logical; if \code{TRUE}, uses the approximate critical value
#' \code{df + sqrt(8 * df)}. If \code{FALSE}, uses the chi-square quantile.
#' @return Dataframe with test type, divergence \emph{D}, chi-square statistic,
#' degrees of freedom, critical value, and decision.
#' @details
#' The function implements four types of tests:
#'
#' \strong{1. Uniformity}
#' \deqn{D = \log r_X - H(X)}
#'
#' \strong{2. Pairwise Independence}
#' \deqn{D = H(X) + H(Y) - H(X,Y)}
#'
#' \strong{3. Conditional Independence}
#' \deqn{D = H(X,Z) + H(Y,Z) - H(Z) - H(X,Y,Z)}
#'
#' where \emph{Z} may also represent a vector of conditioning variables.
#'
#' \strong{4. Nested Model Comparison}
#' \deqn{D = D_{reduced} - D_{full}}
#'
#' The test statistic is
#' \deqn{2nD\log(2),}
#' since entropies are computed using base 2 logarithms.
#'
#' Smaller divergence values indicate better model fit.
#' @author Termeh Shafie
#' @seealso \code{\link{joint_entropy}}, \code{\link{entropy_trivar}}
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data.
#' \emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique}, 129(1), 45-63.
#' @examples
#' data(lawdata)
#' df_att <- lawdata[[4]]
#'
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
#' ## 1. Test uniformity
#' div_gof(att_var, var_uniform = "gender")
#'
#' ## 2. Test pairwise independence
#' div_gof(att_var, var1 = "status", var2 = "gender")
#'
#' ## 3. Test conditional independence
#'
#' ## (a) Conditional independence given a single variable
#' div_gof(att_var,
#'         var1 = "status",
#'         var2 = "gender",
#'         var_cond = "years")
#'
#' ## (b) Conditional independence given multiple variables
#' div_gof(att_var,
#'         var1 = "status",
#'         var2 = "gender",
#'         var_cond = c("years", "age"))
#'
#' ## 4. Nested model comparison
#' ## Compare reduced models against the saturated empirical model.
#' ## The saturated model has divergence D = 0 and df = 0.
#' m_full <- list(D = 0, df = 0)
#'
#' ## (a) Pairwise independence model
#' m_reduced <- div_gof(att_var,
#'                     var1 = "status",
#'                     var2 = "gender")
#'
#' div_gof(att_var,
#'         model_full = m_full,
#'         model_reduced = list(D = m_reduced$D, df = m_reduced$df))
#'
#' ## (b) Conditional independence model
#' m_reduced <- div_gof(att_var,
#'                     var1 = "status",
#'                     var2 = "gender",
#'                     var_cond = "years")
#'
#' div_gof(att_var,
#'         model_full = m_full,
#'         model_reduced = list(D = m_reduced$D, df = m_reduced$df))
#'
#' ## 5. Nested comparison against the saturated empirical model
#' m_full <- list(D = 0, df = 0)
#'
#' m_reduced <- div_gof(att_var,
#'                     var1 = "status",
#'                     var2 = "gender")
#'
#' div_gof(att_var,
#'         model_full = m_full,
#'         model_reduced = list(D = m_reduced$D, df = m_reduced$df))
#' @export

div_gof <- function(dat,
                    var_uniform = NULL,
                    var1 = NULL,
                    var2 = NULL,
                    var_cond = NULL,
                    model_full = NULL,
                    model_reduced = NULL,
                    alpha = 0.05,
                    dec = 3,
                    use_approx_cv = TRUE) {

  entropy_set <- function(dat, vars) {
    tab <- table(dat[, vars, drop = FALSE])
    p <- as.vector(tab) / sum(tab)
    p <- p[p > 0]
    sum(p * log2(1 / p))
  }

  n_levels <- function(x) length(unique(x))

  make_output <- function(test, D, df_chi2) {
    if (df_chi2 <= 0) {
      stop("Degrees of freedom must be positive.")
    }

    chi2_stat <- 2 * nrow(dat) * D * log(2)

    crit_val <- if (use_approx_cv) {
      df_chi2 + sqrt(8 * df_chi2)
    } else {
      stats::qchisq(1 - alpha, df = df_chi2)
    }

    decision <- if (chi2_stat > crit_val) {
      "reject"
    } else {
      "cannot reject"
    }

    data.frame(
      test = test,
      D = round(D, dec),
      chi2 = round(chi2_stat, dec),
      df = df_chi2,
      critical_value = round(crit_val, dec),
      decision = decision
    )
  }

  if (!is.null(var_uniform) &&
      is.null(var1) &&
      is.null(var2) &&
      is.null(var_cond) &&
      is.null(model_full) &&
      is.null(model_reduced)) {

    r_x <- n_levels(dat[[var_uniform]])
    D <- log2(r_x) - entropy_set(dat, var_uniform)
    df_chi2 <- r_x - 1

    return(make_output(
      test = paste0("uniformity: ", var_uniform),
      D = D,
      df_chi2 = df_chi2
    ))
  }

  if (!is.null(var1) &&
      !is.null(var2) &&
      is.null(var_cond) &&
      is.null(model_full) &&
      is.null(model_reduced)) {

    r_x <- n_levels(dat[[var1]])
    r_y <- n_levels(dat[[var2]])

    D <- entropy_set(dat, var1) +
      entropy_set(dat, var2) -
      entropy_set(dat, c(var1, var2))

    df_chi2 <- (r_x - 1) * (r_y - 1)

    return(make_output(
      test = paste0(var1, " independent of ", var2),
      D = D,
      df_chi2 = df_chi2
    ))
  }

  if (!is.null(var1) &&
      !is.null(var2) &&
      !is.null(var_cond) &&
      is.null(model_full) &&
      is.null(model_reduced)) {

    r_x <- n_levels(dat[[var1]])
    r_y <- n_levels(dat[[var2]])
    r_cond <- prod(vapply(dat[var_cond], n_levels, numeric(1)))

    D <- entropy_set(dat, c(var1, var_cond)) +
      entropy_set(dat, c(var2, var_cond)) -
      entropy_set(dat, var_cond) -
      entropy_set(dat, c(var1, var2, var_cond))

    df_chi2 <- (r_x - 1) * (r_y - 1) * r_cond

    return(make_output(
      test = paste0(
        var1, " independent of ", var2,
        " given ", paste(var_cond, collapse = ", ")
      ),
      D = D,
      df_chi2 = df_chi2
    ))
  }

  if (!is.null(model_full) && !is.null(model_reduced)) {
    D <- model_reduced$D - model_full$D
    df_chi2 <- model_reduced$df - model_full$df

    if (df_chi2 <= 0) {
      stop("Invalid nested comparison: `model_reduced$df` must be larger than `model_full$df`.")
    }

    if (D < 0) {
      stop("Invalid nested comparison: `model_reduced$D` must be at least as large as `model_full$D`.")
    }

    return(make_output(
      test = "nested model comparison",
      D = D,
      df_chi2 = df_chi2
    ))
  }

  stop(
    "Specify one of: `var_uniform`, (`var1`, `var2`), ",
    "(`var1`, `var2`, `var_cond`), or (`model_full`, `model_reduced`)."
  )
}

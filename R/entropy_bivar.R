#' @title Bivariate Entropy
#' @description Computes the bivariate entropies between all pairs of (discrete) variables in a multivariate data set.
#' @param dat dataframe with rows as observations and columns as variables. Variables must all be observed or transformed categorical with finite range spaces.
#' @return Upper triangular matrix giving bivariate entropies between pairs of variables given as
#' rows and columns of the matrix. The univariate entropies are given in the diagonal.
#' @details The bivariate entropy \emph{H(X,Y)} of two discrete random variables \emph{X} and \emph{Y}
#' can be used to check for functional relationships and stochastic independence between pairs of variables.
#' The bivariate entropy is bounded according to \cr
#'
#' \emph{H(X) <= H(X,Y) <= H(X) + H(Y)}
#' \cr
#'
#' where \emph{H(X)} and
#' \emph{H(Y)} are the univariate entropies.
#' @author Termeh Shafie
#' @seealso \code{\link{joint_entropy}},  \code{\link{entropy_trivar}}, \code{\link{redundancy}}, \code{\link{prediction_power}}
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data.
#' \emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique}, 129(1), 45-63.
#' @examples
#' # use internal data set
#' data(lawdata)
#' df.att <- lawdata[[4]]
#'
#' # three steps of data editing:
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
#' # calculate bivariate entropies
#' H.biv <- entropy_bivar(df.att.ed)
#' # univariate entropies are then given as
#' diag(H.biv)
#' @export
#'
entropy_bivar <- function(dat) {
    varname_orig <- colnames(dat)
    varname_new <- sprintf("V%d", seq_len(length(dat)))
    names(dat) <- varname_new

    H2 <- matrix(0, nrow = ncol(dat), ncol = ncol(dat))

    # rename columns and rows to match variable names in data frame
    colnames(H2) <- colnames(dat)
    rownames(H2) <- colnames(dat)

    # iterate over all variables in data frame to calculate bivariate entropy
    for (x in seq_len(ncol(dat))) {
        for (y in (x):ncol(dat)) {
            freq <- as.data.frame(table(dat[, x], dat[, y]))
            # frequencies of observations ordered in outcome space
            freq.os <- freq[order(freq[, 1], freq[, 2]), ]
            # calculate bivariate entropies
            H2pos <-
                ifelse(freq.os$Freq > 0, freq.os$Freq * log2(freq.os$Freq), 0)
            H2[x, y] <- log2(nrow(dat)) - (1 / nrow(dat)) * (sum(H2pos))
        }
    }

    colnames(H2) <- varname_orig
    rownames(H2) <- varname_orig
    H2[lower.tri(H2)] <- NA
    H2 <- as.matrix(H2)
    H2 <- round(H2, 3)

    return(H2)
}

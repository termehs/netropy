#' @title Joint Entropy
#' @description Computes the joint entropies between all pairs of (discrete)
#' variables in a multivariate data set.
#' @param dat dataframe with rows as observations and columns as variables.
#' Variables must all be observed or transformed categorical with finite range spaces.
#' @param dec the precision given in number of decimals for which
#' the frequency distribution of unique entropy values is created. Default is 3.
#' @return List with
#' \item{matrix}{an upper triangular joint entropy matrix (univariate entropies in the diagonal).}
#' \item{freq}{a dataframe giving the frequency distributions of unique joint entropy values.}
#' @details The joint entropy \emph{J(X,Y)} of discrete variables \emph{X} and \emph{Y}
#' is a measure of dependence or association between them, defined as
#' \cr
#'
#' \emph{J(X,Y) = H(X) + H(Y) - H(X,Y)}.
#' \cr
#'
#' Two variables are independent if their joint entropy,
#' i.e. their mutual information, is equal to zero.
#' The frequency distributions can be used to decide upon convenient thresholds for
#' constructing association graphs.
#' @author Termeh Shafie
#' @seealso \code{\link{assoc_graph}}, \code{\link{entropy_bivar}}
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
#' # calculate joint entropies
#' J <- joint_entropy(df.att.ed)
#' # joint entropy matrix
#' J$matrix
#' # frequency distribution of joint entropy values
#' J$freq
#' @export
#'
#'

joint_entropy <- function(dat, dec = 3) {
    varname_orig <- colnames(dat)
    varname_new <- sprintf("V%d", seq_len(length(dat)))
    names(dat) <- varname_new

    J <- matrix(0, nrow = ncol(dat), ncol = ncol(dat))
    colnames(J) <- colnames(dat)
    rownames(J) <- colnames(dat)

    # get the bivariate entropies H
    H <- entropy_bivar(dat)

    # joint entropies after calculation of H matrix
    for (x in seq_len(ncol(H))) {
        for (y in (x):ncol(H)) {
            J[x, y] <- H[x, x] + H[y, y] - H[x, y]
        }
    }

    # given input argument dec giving precision, round J
    J <- round(J, dec)
    colnames(J) <- varname_orig
    rownames(J) <- varname_orig
    J[lower.tri(J)] <- NA


    # frequency distribution of the joint entropy values
    FrqJ <- as.data.frame(table(round(J[upper.tri(J, diag = FALSE)], dec)))
    FrqJ <- FrqJ[order(FrqJ$Var1, decreasing = TRUE), ]
    FrqJ$CumFreq <- cumsum(FrqJ$Freq)
    names(FrqJ)[names(FrqJ) == "Var1"] <- "j"
    names(FrqJ)[names(FrqJ) == "Freq"] <- " #(J = j)"
    names(FrqJ)[names(FrqJ) == "CumFreq"] <- "#(J >= j)"
    row.names(FrqJ) <- NULL

    listout <- list("matrix" = J, "freq" = FrqJ)
    return(listout)
}

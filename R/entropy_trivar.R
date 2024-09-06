#' @title Trivariate Entropy
#' @description Computes trivariate entropies of all triples of (discrete)
#' variables in a multivariate data set.
#' @param dat dataframe with rows as observations and columns as variables.
#' Variables must all be observed or transformed categorical with finite range spaces.
#' @return Dataframe with the first three columns representing possible triples of variables (\code{V1,V2,V3})
#' and the fourth column gives trivariate entropies \code{H(V1,V2,V3)}.
#' @details  Trivariate entropies can be used to check for functional relationships and
#' stochastic independence between triples of variables.
#' The trivariate entropy \emph{H(X,Y,Z)} of three discrete random variables \emph{X, Y} and \emph{Z}
#' is bounded according to \cr
#'
#' \emph{H(X,Y) <= H(X,Y,Z) <= H(X,Z) + H(Y,Z) - H(Z)}.
#' \cr
#'
#' The increment between the trivariate entropy and its lower bound is equal to the expected conditional entropy.
#' @author Termeh Shafie
#' @seealso \code{\link{entropy_bivar}}, \code{\link{prediction_power}}
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
#' # calculate trivariate entropies
#' H.triv <- entropy_trivar(df.att.ed)
#' @export

entropy_trivar <- function(dat) {
    varname_orig <- colnames(dat)
    varname_new <- sprintf("V%d", seq_len(ncol(dat)))
    names(dat) <- varname_new

    # call to get bivariate entropies
    H2 <- entropy_bivar(dat)
    colnames(H2) <- varname_new
    rownames(H2) <- varname_new

    # initialize trivariate entropy matrix
    H3 <- matrix(0, nrow = choose(ncol(dat), 3), 4)
    H3 <- data.frame(H3)
    names(H3)[names(H3) == "X1"] <- "V1"
    names(H3)[names(H3) == "X2"] <- "V2"
    names(H3)[names(H3) == "X3"] <- "V3"
    names(H3)[names(H3) == "X4"] <- "H(V1,V2,V3)"

    # iterate over all variables in data frame to calculate trivariate entropies
    k <- 0
    for (x in 1:(ncol(dat) - 2)) {
        for (y in (x + 1):(ncol(dat) - 1)) {
            for (z in (y + 1):ncol(dat)) {
                k <- k + 1
                # create outcome space for triples of variables
                # unq.x <- sort(unique(dat[, x]))
                # unq.y <- sort((unique(dat[, y])))
                # unq.z <- sort((unique(dat[, z])))
                # R <- expand.grid(unq.x, unq.y, unq.z)
                frq <- table(dat[, x], dat[, y], dat[, z]) # frequencies of observations of x and y and z
                # frequencies of observations in outcome space
                frq.os <- as.data.frame(frq)
                Hpos <-
                    ifelse(frq.os$Freq > 0, frq.os$Freq * log2(frq.os$Freq), 0)
                Htmp <- (log2(nrow(dat)) - (1 / nrow(dat)) * (sum(Hpos)))
                H3[k, ] <-
                    c(
                        colnames(dat)[x],
                        colnames(dat)[y],
                        colnames(dat)[z],
                        round(Htmp, 3)
                    )
            }
        }
    }

    H3[["V1"]] <- varname_orig[match(H3[["V1"]], varname_new)]
    H3[["V2"]] <- varname_orig[match(H3[["V2"]], varname_new)]
    H3[["V3"]] <- varname_orig[match(H3[["V3"]], varname_new)]

    H3$`H(V1,V2,V3)` <- as.numeric(H3$`H(V1,V2,V3)`)

    return(H3)
}

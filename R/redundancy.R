#' @title Redundant Variables & Dimensionality Reduction
#' @description Finds redundant variables in a dataframe consisting of discrete variables.
#' @param dat dataframe with rows as observations and columns as variables.
#' Variables must all be observed or transformed categorical with finite range spaces.
#' @param dec  the precision given as number of decimals used to round bivariate entropies
#' in order to find redundant variables (the more decimals, the harder to detect redundancy). Default is 3.
#' @return Binary matrix indicating which row and column variables hold the same information.
#' @details   Redundancy is defined as two variables holding the same information (bivariate entropies)
#' as at least one of the variable alone (univariate entropies).
#' Consider removing one of these two variable from the dataframe for further analysis.
#' @author Termeh Shafie
#' @seealso \code{\link{entropy_bivar}},
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data.
#' \emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique}, 129(1), 45-63.
#' @examples
#' # use internal data set
#' data(lawdata)
#' df.att <- lawdata[[4]]
#'
#' # two steps of data editing:
#' # 1. categorize variables 'years' and 'age' based on
#' # approximately three equally size groups (values based on cdf)
#' # 2. make sure all outcomes start from the value 0 (optional)
#' df.att.ed <- data.frame(
#'     senior = df.att$senior,
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
#' # find redundant variables in dataframe
#' redundancy(df.att.ed) # variable 'senior' should be omitted
#' @export
#'
redundancy <- function(dat, dec = 3) {
    H2 <- entropy_bivar(dat)

    # given input argument dec giving precision, round H2 matrix
    H2 <- round(H2, dec)

    # print out redundant variables
    H2[lower.tri(H2)] <- H2[upper.tri(H2)]
    red <- H2
    for (i in seq_len(ncol(H2))) {
        red[i, ] <- diag(H2)[i] == H2[i, ]
    }
    diag(red) <- 0

    if (sum(red) == 0) {
        message("no redundant variables")
        return()
    }

    return(red)
}

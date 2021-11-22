#' @title Redundant Variables/Dimensionality Reduction
#' @description Finds redundant variables in a dataframe. Redundancy is defined as two variables holding the same information (measured by bivariate entropies).
#' @param dat Dataframe with rows as observations and columns as variables. Variables must all be observed or transformed categorical with finite range spaces.
#' @param dec  The precision given as number of decimals used to round bivariate entropies in order to find redundant variables (the more decimals, the harder to find redundancy). Default is 3.
#' @return Matrix indicating which row and column variables are hold the same information. Consider removing one of these for further analysis.
#' @details  To be completed
#' @author Termeh Shafie
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data. *Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique*, 129(1), 45-63.
#' @examples
#' # to be added
#' @export
#'
redundancy <- function(dat, dec = 3) {
  H2 <- entropy_bivar(dat)

  # given input argument dec giving precision, round H2 matrix
  H2 <- round(H2, dec)

  # print out redundant variables
  H2[lower.tri(H2)] <- H2[upper.tri(H2)]
  red <- H2
  for (i in 1:ncol(H2)) {
    red[i,] = diag(H2)[i] == H2[i,]
  }
  diag(red) = 0

  if (sum(red) == 0) {
    warning('no redundant variables')
    return()
  }

  return(red)
}

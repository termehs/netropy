#' @title Joint Entropies
#' @description Calculates the joint entropies between pairwise variables.
#' @param dat Dataframe with rows as observations and columns as variables. Variables must all be observed or transformed categorical with finite range spaces.
#' @param dec The precision given in number of decimals for which
#' the frequency distribution of unique entropy values is created. Default is 3.
#' @return List with the upper triangular joint entropy matrix (univariate entropy in diagonal) and the frequency distribution of unique joint entropy values,
#' @details  To be completed
#' @author Termeh Shafie
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data. *Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique*, 129(1), 45-63.
#' @examples
#' @export
#'

joint_entropy <- function(dat, dec = 3) {
  varname.orig <- colnames(dat)
  varname.new <- sprintf("V%d", 1:length(dat))
  names(dat) <- varname.new

  J <- matrix(0, nrow = ncol(dat), ncol = ncol(dat))
  colnames(J) = colnames(dat)
  rownames(J) = colnames(dat)

  # get the bivariate entropies H
  H <- entropy_bivar(dat)

  # joint entropies after calculation of H matrix
  for (x in 1:(ncol(H))) {
    for (y in (x):ncol(H)) {
      J[x, y] <- H[x, x] + H[y, y] - H[x, y]
    }
  }

  # given input argument dec giving precision, round J
  J <- round(J, dec)
  colnames(J) <- varname.orig
  rownames(J) <- varname.orig

  # frequency distribution of the joint entropy values
  FrqJ <-
    as.data.frame(table(round(J[upper.tri(J, diag = FALSE)], dec)))
  FrqJ <- FrqJ[order(FrqJ$Var1, decreasing = TRUE), ]
  FrqJ$CumFreq <- cumsum(FrqJ$Freq)
  names(FrqJ)[names(FrqJ) == "Var1"] <- "j"
  names(FrqJ)[names(FrqJ) == "Freq"] <- " #(J = j)"
  names(FrqJ)[names(FrqJ) == "CumFreq"] <- "#(J ≥ j)"
  row.names(FrqJ) <- NULL

  listout <- list("matrix" = J, "freq" = FrqJ)
  return(listout)
}

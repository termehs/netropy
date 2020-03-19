#' @title Bivariate Entropies
#' @description Calculates the bivariate entropies
#' @param dat Dataframe with rows as observations and columns as variables. Variables must all be observed or transformed categorical with finite range spaces.
#' @return Upper triangular matrix giving bivariate entropies between pairs of variables which are given as rows and columns of the matrix.
#' The univariate entropies are given in the diagonal.
#' @details  To be completed
#' @author Termeh Shafie
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data. *Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique*, 129(1), 45-63.
#'
entropy_bivar <- function(dat) {
  varname.orig <- colnames(dat)
  varname.new <- sprintf("V%d", 1:length(dat))
  names(dat) <- varname.new

  H2 <- matrix(0, nrow = ncol(dat), ncol = ncol(dat))

  # rename columnns and rows to match variable names in data frame
  colnames(H2) <- colnames(dat)
  rownames(H2) <- colnames(dat)

  # iterate over all variables in data frame to calculate bivariate entropy
  for (x in 1:(ncol(dat))) {
    for (y in (x):ncol(dat)) {
      # create outcome space for pairs of variables
      unq.x   <- sort(unique(dat[, x]))
      unq.y   <- sort((unique(dat[, y])))
      R       <- expand.grid(unq.x, unq.y)
      freq    <- as.data.frame(table(dat[, x], dat[, y]))
      # frequencies of observations ordered in outcome space
      freq.os <- freq[order(freq[, 1], freq[, 2]),]
      # calculate bivariate entropies
      H2pos    <-
        ifelse(freq.os$Freq > 0, freq.os$Freq * log2(freq.os$Freq), 0)
      H2[x, y]  <- log2(nrow(dat)) - (1 / nrow(dat)) * (sum(H2pos))
    }
  }

  colnames(H2) <- varname.orig
  rownames(H2) <- varname.orig
  H2[lower.tri(H2)] <- NA
  H2 <-  as.matrix(H2)
  H2 <- round(H2, 3)

  return(H2)
}

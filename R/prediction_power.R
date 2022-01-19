#' @title Prediction Power
#' @description 
#' Computes prediction power when pairs of variables in a given dataframe are used 
#' to predict a third variable from the same dataframe. The prediction strength is measured by
#' expected conditional entropies.
#' @param dat dataframe with rows as observations and columns as variables. 
#' Variables must all be observed or transformed categorical with finite range spaces.
#' @param var character string representing the variable in 
#' dataframe \code{dat} to be predicted by pairs of other variables in the dataframe \code{dat}.
#' @return Upper triangular matrix giving the expected conditional entropies of pairs of variables 
#' given as rows and columns of the matrix. The diagonal gives \emph{EH(X|Z) = H(X,Z) - H(Z)} , that is
#' when only one variable is used to predict \code{var}.
#' @details The expected conditional entropy given by\cr
#' 
#' \emph{EH(Z|X,Y) = H(X,Y,Z) - H(X, Y)} \cr
#' 
#' measures the prediction uncertainty when pairs of variables \emph{X} and \emph{Y}
#' are used to predict variable \emph{Z}.
#' The lower the value of \emph{EH} given different pairs of variables \emph{X} and \emph{Y}, 
#' the stronger is the prediction of \emph{Z}.
#' @author Termeh Shafie
#' @seealso \code{\link{entropy_trivar}}, \code{\link{entropy_bivar}}
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data.
#' \emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique}, 129(1), 45-63.
#' \cr
#'
#' Nowicki, K., Shafie, T., & Frank, O. (Forthcoming 2022). \emph{Statistical Entropy Analysis of Network Data}.
#' @examples
#' # use internal data set and the attribute dataframe with 71 observations
#' data(lawdata)

#' @export

prediction_power <- function(var, dat) {
  z <- which(names(dat) == var)
  
  varname.orig <- colnames(dat)
  varname.new <- sprintf("V%d", 1:ncol(dat))
  names(dat) <- varname.new

  H2 <- entropy_bivar(dat)
  H3 <- entropy_trivar(dat)

    
  H3$V1 <- as.numeric(gsub("V", "", H3$V1))
  H3$V2 <- as.numeric(gsub("V", "", H3$V2))
  H3$V3 <- as.numeric(gsub("V", "", H3$V3))
  
  H3 <-  as.matrix(H3)
  dimE <- max(H3[,1:3])
  
  idz = which(apply(H3[,1:3], 1, function(x) any(x==z)))
    EHZXY=matrix(NA,dimE,dimE)
    for(xy in idz){
      idxy=sort(setdiff(H3[xy,1:3],z))
      x=idxy[1]
      y=idxy[2]
      x.coord=which(paste0("V",x)==colnames(H2))
      y.coord=which(paste0("V",y)==colnames(H2))
      z.coord=which(paste0("V",z)==colnames(H2))
      EHZXY[x,y]=H3[xy,4]-H2[x.coord,y.coord]  
      }
  
  #add the diagonal
    z.coord=which(paste0("V",z)==colnames(H2))
    H2[lower.tri(H2)] <- H2[upper.tri(H2)]
    for(x in 1:nrow(H2)){
      if(x!=z){
        EHZXY[x,x]= H2[x,z.coord]-H2[z.coord,z.coord] 
      }
    }
  
    colnames(EHZXY) <- varname.orig
    rownames(EHZXY) <- varname.orig
    EHZXY <-  as.matrix(EHZXY)
    EHZXY <- round(EHZXY, 3)
    
    return(EHZHY)
}

  
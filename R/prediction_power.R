#' @title Prediction Power Based 
#' @description 
#' Computes prediction power when pairs of variables in a given dataframe are used 
#' to predict a third variable from the same dataframe. The prediction strength is measured by
#' expected conditional entropies.
#' @param formula An object of class "formula": a symbolic description of the variable
#' being predicted using pairs of other variables. The details of its specification are given under ‘Details’.
#' @param dat Dataframe with rows as observations and columns as variables. 
#' Variables must all be observed or transformed categorical with finite range spaces.
#' @return 
#' @details The expected conditional entropy given by\cr
#' 
#' \emph{EH(Z|X,Y)= H(X,Y,Z) - H(X, Y) \cr
#' 
#' measures the prediction uncertainty when  pairs of variables \emph{X} and \emph{Y}
#' are used to predict variable \emph{Z}.
#' The lower the value of \emph{EH} given various pairs of variables \emph{X} and \emph{Y}, 
#' the stronger is the prediction of \emph{Z}.
#' @author Termeh Shafie
#' @seealso \code{\link{entropy_trivar}}
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data.
#' \emph{Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique}, 129(1), 45-63.
#' \cr
#'
#' Nowicki, K., Shafie, T., & Frank, O. (Forthcoming 2022). \emph{Statistical Entropy Analysis of Network Data}.
#' @examples
#' # use internal data set and the attribute dataframe with 71 observations
#' data(lawdata)

#' @export

prediction_power <- function(formula, dat) {
  
  # EH(Z|X,Y)=H(X,Y,Z)-H(X,Y) when Z and X and Y differ
  
  
  # if three variables (two predictors)
  # get trivariate entropies
  H3 <- entropy_trivar(dat)
    
  H3$V1 <- as.numeric(gsub("V", "", H3$V1))
  H3$V2 <- as.numeric(gsub("V", "", H3$V2))
  H3$V3 <- as.numeric(gsub("V", "", H3$V3))
  
  # EH given as m matrices with the different Z variables
  
  # H(X,Z)+H(Y,Z)-H(Z)-H(X,Y,Z) 
  H3 <-  as.matrix(H3)
  dimE <- max(H3[,1:3])
  loopZ <- sort(unique(c(H3[ ,1], H3[ ,2], H3[ ,3])))
  EHZXY <- list()
  EJXYZ <- list()
  
  k=0
  for(z in loopZ){
    k=k+1
    idz = which(apply(H3[,1:3], 1, function(x) any(x==z)))
    EHZXY[[k]]=matrix(NA,dimE,dimE)
    EJXYZ[[k]]=matrix(NA,dimE,dimE)
    for(xy in idz){
      idxy=sort(setdiff(H3[xy,1:3],z))
      x=idxy[1]
      y=idxy[2]
      x.coord=which(paste0("V",x)==colnames(H2))
      y.coord=which(paste0("V",y)==colnames(H2))
      z.coord=which(paste0("V",z)==colnames(H2))
      EHZXY[[k]][x,y]=H3[xy,4]-H2[x.coord,y.coord]  
      EJXYZ[[k]][x,y]=H2[x.coord,z.coord]+H2[y.coord,z.coord]-H2[z.coord,z.coord]-H3[xy,4]
    }
  }
  
  #add the diagonal giving EH(X|Z)=H(X,Z)-H(Z) 
  k=0
  for(z in loopZ){
    z.coord=which(str_c("V",z)==colnames(H2))
    k=k+1
    for(x in 1:nrow(H2)){
      if(x!=z){
        EHZXY[[k]][x,x]= H2[x,z.coord]-H2[z.coord,z.coord] 
      }
    }
  }
  
  
# create directory to save EH and EJ outputs
# dir.create("EH")
# dir.create("EJ")

sapply(1:length(EHZXY),function(x) write.table(EHZXY[[x]],str_c("EHZYX_",loopZ[x],".csv"),
                                               sep=",",col.names = F,row.names = F))
sapply(1:length(EJXYZ),function(x) write.table(EHZXY[[x]],str_c("EJXYZ_",loopZ[x],".csv"),
                                               sep=",",col.names = F,row.names = F))



  
}
#' @title Trivariate Entropies
#' @description Calculates the bivariate entropies
#' @param dat Dataframe with rows as observations and columns as variables. Variables must all be observed or transformed categorical variables.
#' @return Dataframe containing all possible triples of variables and their entropies
#' @details  To be completed
#' @author Termeh Shafie
#' @references Frank, O., & Shafie, T. (2016). Multivariate entropy analysis of network data. *Bulletin of Sociological Methodology/Bulletin de Méthodologie Sociologique*, 129(1), 45-63.
#'
#'

entropy_trivar <- function(dat) {
  varname.orig <- colnames(dat)
  varname.new <- sprintf("V%d", 1:length(dat))
  names(dat) <- varname.new

  # calll to get bivariate entropies
  H2 <- entropy_bivar(dat)
  colnames(H2) <- varname.new
  rownames(H2) <- varname.new

  # initialize trivariate entropy matrix
  H3 <- matrix(0, nrow = choose(ncol(dat),3), 4)
  H3 <- data.frame(H3)
  names(H3)[names(H3) == "X1"] <- "V1"
  names(H3)[names(H3) == "X2"] <- "V2"
  names(H3)[names(H3) == "X3"] <- "V3"
  names(H3)[names(H3) == "X4"] <- "H(V1,V2,V3)"

  #iterate over all variables in data frame to calculate trivariate entropies
  k=0
  for(x in 1:(ncol(dat)-2)){
    for(y in (x+1):(ncol(dat)-1)){
      for(z in (y+1):ncol(dat)){
        k=k+1
        #create outcome space for triples of variables
        unq.x <- sort(unique(dat[,x]))
        unq.y <- sort((unique(dat[,y])))
        unq.z <- sort((unique(dat[,z])))
        R <- expand.grid(unq.x, unq.y, unq.z)
        frq <- table(dat[,x], dat[,y], dat[,z]) #frequencies of observations of x and y and z
        #frequencies of observations in outcome space
        frq.os <- as.data.frame(frq)
        Hpos <- ifelse(frq.os$Freq > 0, frq.os$Freq * log2(frq.os$Freq),0)
        Htmp <- (log2(nrow(dat)) - (1 / nrow(dat)) * (sum(Hpos)))
        H3[k, ] <-  c(colnames(dat)[x],  colnames(dat)[y], colnames(dat)[z], round(Htmp,3))
      }
    }
  }

  H3[["V1"]] <- varname.orig[match(H3[["V1"]],varname.new)]
  H3[["V2"]] <- varname.orig[match(H3[["V2"]],varname.new)]
  H3[["V3"]] <- varname.orig[match(H3[["V3"]],varname.new)]

  H3$`H(V1,V2,V3)` <- as.numeric(H3$`H(V1,V2,V3)`)

  return(H3)
}

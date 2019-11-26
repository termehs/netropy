expected_conditional_entropy <- function(dat) {

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

sapply(1:length(EHZXY),function(x) write.table(EHZXY[[x]],str_c("EHZYX_",loopZ[x],".csv"),
                                               sep=",",col.names = F,row.names = F))
sapply(1:length(EJXYZ),function(x) write.table(EHZXY[[x]],str_c("EJXYZ_",loopZ[x],".csv"),
                                               sep=",",col.names = F,row.names = F))
}

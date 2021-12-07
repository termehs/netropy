



get_triad_var <- function(var, type = 'att') {
  if (type == 'att') {
    n <- length(var)
    triads <- t(combn(1:n,3))
    triads <- cbind(triads,var[triads[,1]], var[triads[,2]],var[triads[,3]])
    triad.var <- data.frame(u=triads[,1], v=triads[,2], w=triads[,3],
                            u.var=triads[,4], v.var=triads[,5], w.var=triads[,6])
    triad.var$uvw <- paste(triad.var$u.var,
                           triad.var$v.var,
                           triad.var$w.var)
    var.matching <- apply(expand.grid(min(var):max(var),
                                      min(var):max(var),
                                      min(var):max(var)),1, paste,collapse=" ")
    triad.var$var <- match(triad.var$uvw,var.matching)-1
    if (length(unique(var))>=7) {
      warning('consider categorizing variable as it yields too many dyadic outcomes')
    }
  }
  else if (type == 'tie') {
 n <- dim(var)[1]
    if (isSymmetric(var) == TRUE) {
      triads <- t(combn(1:n,3))
      triads <- cbind(triads,
                      var[triads[,c(1,2)]],
                      var[triads[,c(1,3)]],
                      var[triads[,c(2,3)]]
      )
      colnames(triads) <- c("u","v","w",
                            "uv","uw","vw")
      triad.var <- as.data.frame(triads)
      triad.var$uv.uw.vw <- apply(triad.var[,4:6],1,
                                  paste,collapse=" ")
      grid <- expand.grid(0:1,0:1,0:1)
      var.matching <- apply(grid,1,
                            function(x) paste(x,collapse=" "))
      triad.var$var <- match(triad.var$uv.uw.vw,var.matching)-1
      message('8 outcomes based on triples of indicators for the undirected relation is created')
    }
    else if (isSymmetric(var) == FALSE) {
      triads <- t(combn(1:n,3))
      triads <- cbind(triads, var[triads[,c(1,2)]],
                              var[triads[,c(2,1)]],
                              var[triads[,c(1,3)]],
                              var[triads[,c(3,1)]],
                              var[triads[,c(2,3)]],
                              var[triads[,c(3,2)]])
      colnames(triads) <- c("u","v","w",
                            "uv","vu","uw","wu","vw","wv")
      triad.var <- as.data.frame(triads)
      triad.var$seq <- apply(triad.var[,4:9],1,
                             paste,collapse=" ")
      grid <- expand.grid(0:1,0:1,0:1,0:1,0:1,0:1)
      var.matching <- apply(grid,1,
                            function(x) paste(x,collapse=" "))
      triad.var$var <- match(triad.var$seq,var.matching)-1
      message('64 outcomes based on a sequence of 6 indicators for the directed relation is created')
    }
  }

  out.var <- as.data.frame(cbind(triad.var$u, triad.var$v, triad.var$w, triad.var$var))
  colnames(out.var) <- c("u","v","w","var")
  return(out.var)
}

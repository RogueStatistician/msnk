#' @export
directional.sample <- function(X){
  # p <- NCOL(X)
  # n <- NROW(X)
  # Xc<-scale(X)
  # J24 <- 3/(p*(p+2)*(p+4))
  # J6 <- 15/(p*(p+2)*(p+4))
  # J2 <- 1/p
  # K<-(9*(2+3*p))/((p+2)*(p+4))
  # T <- rep(0,p)
  # for(i in 1:p){
  #   T[i]<-J6*mean(Xc[,i]^4)
  #   for(j in 1:p){
  #     if(j!=i)
  #       T[i]<-T[i]+6*J24*mean(Xc[,i]^2*Xc[,j]^2)+J24*mean(Xc[,j]^4)
  #   }
  #   T[i]<-T[i]-J2*K
  # }
  # return(list("T"=T,"Q"=t(T)%*%solve(var(Xc))%*%T))
  xx<-selm(X~1,family = 'SN')
  return(directional(xx@param$dp$Omega,alpha=xx@param$dp$alpha))
}

# Indice scalare t(Tn)%*%solve(O)%*%Tn
# Plot3d per i countour 3d se non trovo altro

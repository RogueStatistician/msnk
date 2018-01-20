#' @export
kollo.sample <- function(X){
  # p <- NCOL(X)
  # n <- NROW(X)
  # Xc<-scale(X)
  # #colMeans(Xc^4)
  # yty<-t(Xc)%*%Xc
  # mm<-0
  # for(i in 1:p){
  #   for(j in 1:p)
  #     mm<-mm+sum(Xc[,i]*Xc[,j])*yty
  # }
  # mm/(p*n^2)

  xx<-selm(X~1,family = 'SN')
  return(kollo(xx@param$dp$Omega,alpha=xx@param$dp$alpha))
}


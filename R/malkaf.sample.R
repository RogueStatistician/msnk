#' @export
malkaf.sample<-function(X){
  p <- NCOL(X)
  n <- NROW(X)
  Xc<-scale(X)
  med<-apply(Xc,2,mean)
  va<-var(Xc)
  mm<-rep(0,p)
  for(i in 1:p){
    mm[i]<-(mean((Xc[,i]-med[i])^4)/va[i,i]^2-3)^2
  }
  max(mm)-9
}

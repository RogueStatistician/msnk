#' @export
mardia.sample<-function(X){
  p <- NCOL(X)
  n <- NROW(X)
  Xc<-scale(X)
  med<-apply(Xc,2,mean)
  va<-var(Xc)
  mm<-sum(diag(((Xc-med)%*%solve(va)%*%t(Xc-med))^2),na.rm=T)
  mm/n-p*(p+2)
}

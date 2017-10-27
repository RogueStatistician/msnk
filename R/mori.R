#' @export
mori.rohatgi.szekeley <- function(Omega,xi=rep(0,length(alpha)),alpha){
  O <- cov2cor(Omega)
  p<- nrow(Omega)
  Ip<- diag(p)
  delta <- O%*%alpha%*%(1+t(alpha)%*%O%*%alpha)^(-1/2)
  xi <- xi+sqrt(2/pi)*delta
  K<-Ip%s%M4(O,-xi,alpha)-(p+2)*diag(p)
  return(K)
}


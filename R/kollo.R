#' @export
kollo <- function(Omega,xi=rep(0,length(alpha)),alpha){
  O <- cov2cor(Omega)
  p<- nrow(Omega)
  One.p<- matrix(rep(1,p^2),p,p)
  delta <- O%*%alpha%*%(1+t(alpha)%*%O%*%alpha)^(-1/2)
  xi <- xi+sqrt(2/pi)*delta
  K<-One.p%s%M4(O,-xi,alpha)
  return(K)
}

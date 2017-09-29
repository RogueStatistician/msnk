#' @export
kollo <- function(Omega,xi=rep(0,length(alpha)),alpha){
  O <- cov2cor(Omega)
  p<- nrow(Omega)
  One.p<- matrix(rep(1,p^2),p,p)
  K<-One.p%s%M4(O,xi,alpha)
  return(K)
}

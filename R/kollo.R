#' @export
kollo <- function(Omega,xi,alpha){
  p<- nrow(Omega)
  Ip<- matrix(rep(1,p^2),p,p)
  K<-Ip%s%M4(Omega,xi,alpha)
  return(K)
}

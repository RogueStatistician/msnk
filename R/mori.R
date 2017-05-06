#' @export
mori.rohatgi.szekeley <- function(Omega,xi,alpha){
  p<- nrow(Omega)
  Ip<- diag(p)
  K<-Ip%s%M4(Omega,xi,alpha)-(p+2)*diag(p)
  return(K)
}

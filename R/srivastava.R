#' @export
kollo <- function(Omega,xi,alpha){
  p <- nrow(Omega)
  Ip<- matrix(rep(1,p^2),p,p)
  m <- M4(Omega,xi,alpha)
  dec <- eigen(Omega)
  E<- apply(
  X = dec$vectors,
  FUN = function(gamma){
    (t(gamma)%x%t(gamma))%*%m%*%(gamma%x%gamma)
  },
  MARGIN = 2)
  return(1/p*sum(E/dec$values^2))
}

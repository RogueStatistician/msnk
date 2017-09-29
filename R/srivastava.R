#' @export
srivastava <- function(Omega,xi=rep(0,length(alpha)),alpha){
  p <- nrow(Omega)
  m <- M4(Omega,xi,alpha)
  dec <- eigen(Omega)
  E<- apply(
  X = dec$vectors,
  FUN = function(gamma){
    (t(gamma)%x%t(gamma))%*%m%*%(gamma%x%gamma)
  },
  MARGIN =  2)
  return(1/p*sum(E/dec$values^2))
}

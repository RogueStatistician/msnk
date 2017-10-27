#' @export
srivastava <- function(Omega,xi=rep(0,length(alpha)),alpha){
  p <- nrow(Omega)
  delta <- O%*%alpha%*%(1+t(alpha)%*%O%*%alpha)^(-1/2)
  xi <- xi+sqrt(2/pi)*delta
  m <- M4(O,-xi,alpha)
  dec <- eigen(O)
  E<- apply(
  X = dec$vectors,
  FUN = function(gamma){
    (t(gamma)%x%t(gamma))%*%m%*%(gamma%x%gamma)
  },
  MARGIN =  2)

  return(1/p*sum(E/dec$values^2))
}

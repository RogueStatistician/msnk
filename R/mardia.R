mardia <- function(Omega,alpha){
  O<-cov2cor(Omega)
  mu<-sqrt(2/pi)*O%*%alpha%*%(1+t(alpha)%*%O%*%alpha)^(-1/2)
  k<-t(mu)%*%solve(O)%*%mu
  return(2*(pi-3)*(k)/(1+k))
}

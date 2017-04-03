#' Univariate kurtosis indices
#' @aliases mardia malkaf
#' @usage univar.krutosis(Omega,alpha,type=c('mardia','malkovich-afifi'))
#' mardia(Omega,alpha)
#' malkaf(Omega,alpha)
#' @param Omega a symmetric positive-definite matrix of dimension (d,d)
#' @param alpha a numeric vector which regulates the slant of the density
#' @param type univariate index chocice, must be 'malkovich-afifi' or 'mardia'. Not required in mardia and malkaf aliases
#' @return The univariate kurtosis index.
#' @export
univar.kurtosis<- function(Omega,alpha,type='mardia'){
  O<-cov2cor(Omega)
  mu<-sqrt(2/pi)*O%*%alpha%*%(1+t(alpha)%*%O%*%alpha)^(-1/2)
  k<-t(mu)%*%solve(O)%*%mu
  r <- 2*(pi-3)*(k)/(1+k)
  if(type=='malkovich-afifi') r<-r^2
  return(r)
}


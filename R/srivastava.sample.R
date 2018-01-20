#' @export
srivastava.sample <- function(X){
  p <- NCOL(X)
  n <- NROW(X)
  xx<-selm(X~1,family='SN')
  omega<- xx@param$dp$Omega
  alpha <- xx@param$dp$alpha
  O <- cov2cor(omega)
  delta <- O%*%alpha%*%(1+t(alpha)%*%O%*%alpha)^(-1/2)
  med<- sqrt(2/pi)*delta#apply(Xc,2,mean)
  omegaz<- eigen(omega)
  #center<- apply((omegaz$vectors%*%t(X-matrix(rep(t(med),n),ncol=p,byrow = T)))^4,1,mean)
  Xc<-t(solve(diag(diag(Omega)))%*%t(X-matrix(rep(t(med),n),ncol=p,byrow = T)))

  val<-sum(center/omegaz$values^2)
  return(1/(p)*val)
}

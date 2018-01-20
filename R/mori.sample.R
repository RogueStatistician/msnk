#' @export
mori.rohatgi.szekeley.sample <- function(X){
  p <- NCOL(X)
  n <- NROW(X)
  xx<-selm(X~1,'SN')
  Omega<- xx@param$dp$Omega
  alpha <- xx@param$dp$alpha
  xi <- xx@param$dp$beta
  O<-cov2cor(Omega)
  delta <- O%*%alpha%*%(1+t(alpha)%*%O%*%alpha)^(-1/2)
  med<- t(xi)+sqrt(2/pi)*delta
  Xc<-t(solve(diag(diag(Omega)))%*%t(X-matrix(rep(t(med),n),ncol=p,byrow = T)))
  m<-1/n^2*(t(Xc%x%Xc)%*%(Xc%x%Xc))
  K<-diag(p)%s%m-(p+2)*diag(p)
  return(K)
#
#   xx<-selm(X~1,family = 'SN')
#   return(mori.rohatgi.szekeley(xx@param$dp$Omega,alpha=xx@param$dp$alpha))
}


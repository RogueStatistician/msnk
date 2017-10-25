#' @export
directional <- function(Omega,xi=rep(0,length(alpha)),alpha,K=0){
  p <- NROW(Omega)
  delta <- c((Omega%*%alpha)/sqrt(1+t(alpha)%*%Omega%*%alpha)[1,1])
  J24 <- 3/(p*(p+2)*(p+4))
  J6 <- 15/(p*(p+2)*(p+4))
  J2 <- 1/p
  T <- rep(0,p)
  for(i in 1:p){
    T[i]<-J6*
      (
        6*Omega[i,i]*xi[i]^2+xi[i]^4+sqrt(2/pi)*delta[i]*xi[i]*
        (
         12*Omega[i,i]+3*xi[i]^2+delta[i]*xi[i]-4*delta[i]
         )
       )
    for(j in 1:p){
      if(j!=p){
        T[i]<-T[i]+
          J24*(
            4*Omega[j,i]*xi[j]*xi[i]+Omega[j,j]*xi[i]^2+xi[i]^2*xi[j]^2+
            2*Omega[j,i]^2+Omega[i,i]*Omega[j,j]+Omega[j,j]*xi[j]^2+sqrt(2/pi)*
            (
              4*delta[j]*Omega[j,i]*xi[i]+2*delta[i]*Omega[j,j]*xi[i]+2*delta[j]*xi[j]*xi[i]^2+
              delta[i]*xi[j]^2*xi[i]+4*delta[i]*Omega[j,i]*xi[j]+2*delta[j]*Omega[j,j]*xi[j]+
              delta[j]*delta[i]*xi[j]*xi[i]-2*delta[j]^2*delta[i]*xi[i]-2*delta[j]*delta[i]^2*xi[j]
            )
          )
      }
      if(j!=p){
        T[i]<-T[i]+
          J24*(
            6*Omega[j,j]*xi[j]^2+xi[j]^4+sqrt(2/pi)*delta[j]*xi[j]*
            (12*Omega[j,j]+3*xi[j]^2+delta[j]*xi[j]-4*delta[j]
            )
          )
      }

    }
    T[i]<-T[i]-J2*K
  }

  return(T)
}

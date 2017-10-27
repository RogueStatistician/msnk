#' @export
directional <- function(Omega,xi=rep(0,length(alpha)),alpha,K=0){
  p <- NROW(Omega)
  O<-cov2cor(Omega)
  delta <- O%*%alpha%*%(1+t(alpha)%*%O%*%alpha)^(-1/2)
  xi <- xi + sqrt(2/pi)*delta
  J24 <- 3/(p*(p+2)*(p+4))
  J6 <- 15/(p*(p+2)*(p+4))
  J2 <- 1/p
  T <- rep(0,p)
  for(i in 1:p){
    T[i]<-J6*
      (
        6*O[i,i]*xi[i]^2+xi[i]^4+sqrt(2/pi)*delta[i]*xi[i]*
        (
         12*O[i,i]+3*xi[i]^2+delta[i]*xi[i]-4*delta[i]
         )
       )
    for(j in 1:p){
      if(j!=p){
        T[i]<-T[i]+
          6*J24*(
            4*O[j,i]*xi[j]*xi[i]+O[j,j]*xi[i]^2+xi[i]^2*xi[j]^2+
            2*O[j,i]^2+O[i,i]*O[j,j]+O[j,j]*xi[j]^2+sqrt(2/pi)*
            (
              4*delta[j]*O[j,i]*xi[i]+2*delta[i]*O[j,j]*xi[i]+2*delta[j]*xi[j]*xi[i]^2+
              delta[i]*xi[j]^2*xi[i]+4*delta[i]*O[j,i]*xi[j]+2*delta[j]*O[j,j]*xi[j]+
              delta[j]*delta[i]*xi[j]*xi[i]-2*delta[j]^2*delta[i]*xi[i]-2*delta[j]*delta[i]^2*xi[j]
            )
          )
      }
        if(j!=p){

        T[i]<-T[i]+
          J24*(
            6*O[j,j]*xi[j]^2+xi[j]^4+3*O[j,j]+sqrt(2/pi)*delta[j]*xi[j]*
            (12*O[j,j]+3*xi[j]^2+delta[j]*xi[j]-4*delta[j]
            )
          )
      }

    }
    T[i]<-T[i]-J2*K
  }
  return(T)
}

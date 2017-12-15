#' @export
directional <- function(Omega,xi=rep(0,length(alpha)),alpha,K=0){
  p <- NROW(Omega)
  O<-cov2cor(Omega)
  delta <- O%*%alpha%*%(1+t(alpha)%*%O%*%alpha)^(-1/2)
  xi2 <- -(xi + sqrt(2/pi)*delta)
  J24 <- 3/(p*(p+2)*(p+4))
  J6 <- 15/(p*(p+2)*(p+4))
  J2 <- 1/p
  T <- rep(0,p)
  for(i in 1:p){
    T[i]<-J6*
      (
        6*O[i,i]*xi2[i]^2+xi2[i]^4+3*O[i,i]+sqrt(2/pi)*delta[i]*xi2[i]*
        (
         12*O[i,i]+3*xi2[i]^2+delta[i]*xi2[i]-4*delta[i]
         )
       )
    for(j in 1:p){
      if(j!=i){
        T[i]<-T[i]+
          6*J24*(
            4*O[i,j]*xi2[i]*xi2[j]+O[i,i]*xi2[i]^2+xi2[i]^2*xi2[j]^2+
            2*O[i,j]^2+O[i,i]*O[j,j]+O[i,i]*xi2[i]^2+sqrt(2/pi)*
            (
              4*delta[i]*O[i,j]*xi2[j]+2*delta[j]*O[i,j]*xi2[j]+2*delta[i]*xi2[i]*xi2[j]^2+
              delta[j]*xi2[i]^2*xi2[j]+4*delta[j]*O[i,j]*xi2[i]+2*delta[i]*O[i,j]*xi2[i]+
              delta[j]*delta[i]*xi2[j]*xi2[i]-2*delta[i]^2*delta[j]*xi2[j]-2*delta[i]*delta[j]^2*xi2[i]
            )
          )+J24*(
            6*O[j,j]*xi2[j]^2+xi2[j]^4+3*O[j,j]+sqrt(2/pi)*delta[j]*xi2[j]*
            (12*O[j,j]+3*xi2[j]^2+delta[j]*xi2[j]-4*delta[j]
            )
          )
      }

    }
    T[i]<-T[i]-J2*K+(6-9*p+3*p^2)/(p*(p+2)*(p+4))
  }
  return(list("T"=T,"Q"=t(T)%*%solve(O)%*%T))
}

# Indice scalare t(Tn)%*%solve(O)%*%Tn
# Plot3d per i countour 3d se non trovo altro

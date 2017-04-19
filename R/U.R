perm<-function(n,m){
  E <- matrix(rep(0,n*m),nrow = n,ncol = m)
  U <- matrix(rep(0,(n*m)^2),nrow = n*m,ncol = n*m)
  for(i in 1:n){
    for(j in 1:m){
      E[i,j]<- 1
      U <- U + E%x%t(E)
      E[i,j]<- 0
    }
  }
  return(U)
}

#' @export
`%s%` <- function(A,B) {
  if(typeof(A)!='matrix') A<-as.matrix(A)
  if(typeof(B)!='matrix') B<-as.matrix(B)
  p <- nrow(B)/nrow(A)
  q <- ncol(B)/ncol(A)
  res <- matrix(rep(0,p*q),nrow = p,ncol=q)
  for(i in 0:(nrow(A)-1)){
    for(j in 0:(ncol(A)-1)){
      B.sub<-B[(1+i*p):((i+1)*p),(1+j*q):((j+1)*q)]
      res<-res+A[i+1,j+1]*as.matrix(B.sub)
    }
  }
  return(res)
}

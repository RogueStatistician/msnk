#' @export
J <- function(m1=0,m2=0){
  if(m1<0 || m2<0){
    print('[ERROR]: Arguments must be positive')
    return(NaN)
  }
  if(m1+m2>6){
    print('[ERROR]: Sum of arguments must be 6')
    return(NaN)
  }

}

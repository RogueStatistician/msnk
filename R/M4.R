M4 <- function(Omega,xi=rep(0,length(alpha)),alpha){
  delta <-c((Omega%*%alpha)/sqrt(1+t(alpha)%*%Omega%*%alpha)[1,1])
  p <- nrow(Omega)
  return(
    Omega %x% xi %x% t(xi)       +
    xi %x% Omega %x% t(xi)       +
    c(Omega) %x% t(xi) %x% t(xi) +
    xi %x% t(xi) %x% xi %x% t(xi)+
    Omega %x% Omega              +
    c(Omega) %*% t(c(Omega))     +
    perm(p,p)                  %*%
    (Omega %x% Omega)            +
    t(xi) %x% Omega %x% xi       +
    xi %x% xi %x% t(c(Omega))    +
    xi %x% t(xi) %x% Omega       +
    sqrt(2/pi)                   *
    (
      delta %x% Omega %x% t(xi)            +
      c(Omega) %x% t(delta) %x% t(xi)      +
      ((diag(p)%x%delta)%*%Omega)%x% t(xi) +
      delta%x%t(xi)%x%xi%x%t(xi)           +
      xi%x%t(delta)%x%xi%x%t(xi)           +
      xi%x%t(xi)%x%delta%x%t(xi)           +
      t(delta)%x%Omega%x%xi                +
      delta%x%t(c(Omega))%x%xi             +
      (Omega%*%(diag(p)%x%t(delta)))%x% xi +
      t(xi)%x%delta%x%Omega                +
      t(xi)%x%(c(Omega)%*%t(delta))        +
      t(xi)%x%((diag(p)%x%delta)%*%Omega)  +
      xi%x%t(delta)%x%Omega                +
      xi%x%delta%x%t(c(Omega))             +
      xi%x%(Omega%*%(diag(p)%x%t(delta)))  +
      xi%x%t(xi)%x%delta%x%t(delta)        -
      delta%x%t(delta)%x%delta%x%t(xi)     -
      t(delta)%x%delta%x%t(delta)%x%xi     -
      t(xi)%x%delta%x%t(delta)%x%delta     -
      xi%x%t(delta)%x%delta%x%t(delta)
    )
  )
}

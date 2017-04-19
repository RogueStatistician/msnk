mori.rohatgi.szekeley <- function(Omega,xi,delta){
  p<- nrow(Omega)
  Ip<- diag(p)
  K<-Ip%s%(
    Omega%x%xi%x%t(xi)+
    xi%x%Omega%x%t(xi)+
    c(Omega)%X%t(xi)%x%t(xi)+
    xi%x%t(xi)%x%xi%x%t(xi)+
    Omega%x%Omega+
    c(Omega)%*%t(c(Omega))+

    )
}

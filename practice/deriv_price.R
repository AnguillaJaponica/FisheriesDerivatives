deriv_price <- function(r,u,d,N,dt){ 
  q <- (r*dt-d)/(u-d)#risk neutral measure
  C0 <- 0 #derivatives price 0
  
  for(j in 0:N){
    C0 <- C0 + 10*j*choose(N,j)*q^j*(1-q)^(N-j)/(1+r*dt)^N
  }
  return (c("Risk neutral measure"=q,"Derivatives price"=C0))
}

deriv_price(0.02,0.1,-0.09,5,0.2)
  
  

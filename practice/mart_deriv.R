mart_deriv <- function(r,u,d,Cu,Cd){
  q <- (r-d)/(u-d)
  C0 <- (q*Cu + (1-q)*Cd/(1+r)) #リスク中立測度による期待値
  return (c("Risk_Neutral_Measure"=q,"Derivatives_Price"=C0))
}

r = 0.02
u = 0.1
d = -0.09
Cu = 50
Cd = 0

mart_deriv(r,u,d,Cu,Cd)

repli <- function(r,u,d,S){
  #Arrow_debreu証券phi_uの複製PFを求める。
  phi_u_S <- 1/((u-d)*S)#リスク資産の保有量
  phi_u_B <- -(1+d)/((1+r)*(u-d))#安全資産の保有量
  
  #Arrow_debreu証券phi_dの複製PFを求める。
  phi_d_S <- -1/((u-d)*S)#リスク資産の保有量
  phi_d_B <- (1+u)/((1+r)*(u-d))#安全資産の保有量
  
  return (rbind("phi_u_PF"=c(phi_u_S,phi_u_B),"phi_d_PF"=c(phi_d_S,phi_d_B)))
}

repli(0.1,0.2,-0.1,100)  


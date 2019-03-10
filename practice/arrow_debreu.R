arrow_debreu <- function(r,u,d)
{
  phi_u <- (r-d) / ((1+r)*(u-d))
  phi_d <- (u-r) / ((1+r)*(u-d))
  return (c(phi_u,phi_d))
}

r <- 0.1
u <- 0.2
d <- -0.1

arrow_debreu(r,u,d)


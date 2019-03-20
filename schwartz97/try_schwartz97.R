#setwd("/Users/takaterumasa/FisheriesDerivatives")
library("mvtnorm") #先にインストールしておいてください
source("schwartz97classes.R")
source("schwartz97fit.R")
source("schwartz97generics.R")
source("schwartz97internals.R")
source("schwartz97methods.R")
source("schwartz97pricing.R")

##===============変数を入力しよう===============##
s0 <- 56.34 #時点0におけるスポット価格
delta0 <- 0.05 #時点0におけるコンビニエンスイールド
mu <- 0.1 #スポット価格におけるドリフト項のパラメータμ
sigmaS <- 0.3 #スポット価格における拡散項のパラメータ σS
kappa <- 1.5 #コンビニエンスイールドの平均回帰スピード
alpha <- 0.05 #コンビニエンスイールドの平均準位
sigmaE <- 0.4 #コンビニエンスイールドにおける拡散項のパラメータ σE
rho <- 0.6 #2つのブラウン運動の相関係数
time <- 5
##===============変数を入力しよう===============##
obj <- schwartz2f(s0 = s0, delta0 = delta0, alpha = alpha,mu = mu, sigmaS = sigmaS, sigmaE = sigmaE,rho = rho, kappa = kappa)
sample.t <- rstate(n = 2000, time, obj)
pstate(c(0, -Inf), c(150, 0), time, obj)
mean(obj, time = c(1, 10))
plot(obj, n = 30, time = 5, dt = 1 / 52)

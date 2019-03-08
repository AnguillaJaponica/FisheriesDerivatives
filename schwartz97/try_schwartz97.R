library("mvtnorm") #先にインストールしてください
source("schwartz97classes.R")
source("schwartz97fit.R")
source("schwartz97generics.R")
source("schwartz97internals.R")
source("schwartz97methods.R")

##===============変数を入力しよう===============##
s0 <- 100 #時点0におけるスポット価格
delta0 <- 0 #時点0におけるコンビニエンスイールド
mu <- 0.1 #スポット価格におけるドリフト項のパラメータμ
sigmaS <- 0.2 #スポット価格における拡散項のパラメータ σS
kappa <- 1 #コンビニエンスイールドの平均回帰スピード
alpha <- 0.1 #コンビニエンスイールドの平均準位
sigmaE <- 0.3 #コンビニエンスイールドにおける拡散項のパラメータ σE
rho <- 0.4 #2つのブラウン運動の相関係数
##===============変数を入力しよう===============##

obj <- schwartz2f(s0 = s0, delta0 = delta0, alpha = alpha,mu = mu, sigmaS = sigmaS, sigmaE = sigmaE,rho = rho, kappa = kappa)
obj

time <- 10
sample.t <- rstate(n = 2000, time, obj)
pstate(c(0, -Inf), c(150, 0), time, obj)
mean(obj, time = c(1, 10))
plot(obj, n = 30, time = 5, dt = 1 / 52)

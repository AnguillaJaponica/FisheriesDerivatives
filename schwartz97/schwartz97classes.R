#シュワルツの2項モデル・クラス定義

setClass("schwartz2f", #シュワルツの2項モデルの定義付け
         #変数の型定義
         representation(call = "call", #関数呼び出し
                        s0 = "numeric",#時点0におけるスポット価格
                        delta0 = "numeric",#時点0におけるコンビニエンスイールド
                        mu = "numeric", #スポット価格におけるドリフト項のパラメータμ
                        sigmaS = "numeric",#スポット価格における拡散項のパラメータ σS
                        kappaE = "numeric",#コンビニエンスイールドの平均回帰スピード
                        alpha = "numeric",#コンビニエンスイールドの平均準位
                        sigmaE = "numeric",#コンビニエンスイールドにおける拡散項のパラメータ σE
                        rhoSE = "numeric"),#2つのブラウン運動の相関係数
         prototype(call = call("schwartz2f"), #初期値の設定
                   s0 = 100,
                   delta0 = 0,
                   mu = 0,
                   sigmaS = 0.3,
                   kappaE = 1,
                   alpha = 0,
                   sigmaE = 0.3,
                   rhoSE = 0.5)
         )


setClass("schwartz2f.fit",#シュワルツ2項モデルのパラメータ推定にかかる定義付け
         representation(n.iter = "numeric",#繰り返しの数
                        llh = "numeric",#対数尤度値
                        converged = "logical",#収束しているか否か
                        error.code = "numeric", #エラーコード/0
                        error.message = "character",#エラーメッセージ
                        fitted.params = "logical",#パラメータがフィットしたかどうか
                        trace.pars = "matrix",#推定する間におけるパラメータ進化(parameter evolution)
                        r = "numeric",#リスクフリーレート
                        alphaT = "numeric",#リスク中立確率Qにおけるコンビニエンスイールドの平均準位
                        lambdaE = "numeric",#コンビニエンスイールドの市場価格
                        meas.sd = "numeric",#measurement equationの標準偏差
                        deltat = "numeric"),#状態遷移方程式の時間増加
         #変数の型定義
         prototype(n.iter = numeric(0),
                   llh = numeric(0),
                   converged = FALSE,
                   error.code = 0,
                   error.message = character(0),
                   fitted.params = logical(0),
                   trace.pars = matrix(0),
                   r = 0.05,
                   alphaT = 0,
                   lambdaE = 0,
                   meas.sd = 0.01,
                   deltat = 0.01),
         contains = "schwartz2f"
         )

### <---------------------------------------------------------------------->

schwartz2f <- function(s0 = 100, delta0 = 0,
                       mu = 0.1, sigmaS = 0.3,
                       kappa = 1, alpha = 0, sigmaE = 0.3,
                       rho = 0.5)
{
  call <- match.call()
  ## <------------------------------------------------------------>
  ## Check input
  ## <------------------------------------------------------------>

  ## ベクトル
  if(!is.vector(s0)){ #S0がベクトルでない場合停止
    stop("'S0' must be a vector!")
  }
  if(!is.vector(delta0)){#delta0がベクトルでない場合停止
    stop("'delta0' must be a vector!")
  }
  if(!is.vector(mu)){#muがベクトルでない場合停止
    stop("'mu' must be a vector!")
  }
  if(!is.vector(sigmaS)){#σSがベクトルでない場合停止
    stop("'sigmaS' must be a vector!")
  }
  if(!is.vector(kappa)){#kappaがベクトルでない場合停止
    stop("'kappa' must be a vector!")
  }
  if(!is.vector(alpha)){#alphaがベクトルでない場合停止
    stop("'alpha' must be a vector!")
  }
  if(!is.vector(sigmaE)){#sigmaEがベクトルでない場合停止
    stop("'sigmaE' must be a vector!")
  }
  if(!is.vector(rho)){#rhoがベクトルでない場合停止
    stop("'rho' must be a vector!")
  }

  ## 有限の数値:
  if(!is.finite(s0) | !is.numeric(s0)){   # 有限の数値かどうかチェック
    stop("'S0' must be numeric and finite!")
  }
  if(!is.finite(delta0) | !is.numeric(delta0)){
    stop("'delta0' must be numeric and finite!")
  }
  if(!is.finite(mu) | !is.numeric(mu)){
    stop("'mu' must be numeric and finite!")
  }
  if(!is.finite(sigmaS) | !is.numeric(sigmaS)){
    stop("'sigmaS' must be numeric and finite!")
  }
  if(!is.finite(kappa) | !is.numeric(kappa)){
    stop("'kappa' must be numeric and finite!")
  }
  if(!is.finite(alpha) | !is.numeric(alpha)){
    stop("'alpha' must be numeric and finite!")
  }
  if(!is.finite(sigmaE) | !is.numeric(sigmaE)){
    stop("'sigmaE' must be numeric and finite!")
  }
  if(!is.finite(rho) | !is.numeric(rho)){
    stop("'rho' must be numeric and finite!")
  }

  ## 大きさ:
  if(length(s0) != 1){
    stop("'S0' must have length 1!")#大きさが1かどうかチェック
  }
  if(length(delta0) != 1){
    stop("'delta0' must have length 1!")
  }
  if(length(mu) != 1){
    stop("'mu' must have length 1!")
  }
  if(length(sigmaS) != 1){
    stop("'sigmaS' must have length 1!")
  }
  if(length(kappa) != 1){
    stop("'kappa' must have length 1!")
  }
  if(length(alpha) != 1){
    stop("'alpha' must have length 1!")
  }
  if(length(sigmaE) != 1){
    stop("'sigmaE' must have length 1!")
  }
  if(length(rho) != 1){
    stop("'rho' must have length 1!")
  }

  ##特殊な場合
  if(sigmaE <= 0){ 
    stop("'sigmaE' must be greater than 0!")
  }
  if(sigmaS <= 0){
    stop("'sigmaS' must be greater than 0!")
  }
  if(kappa <= 0){
    stop("'kappa' must be greater than 0!")
  }

  # schwartz2f.fitクラスのオブジェクトを返す。
  # このクラスはschwartz2fクラスを継承し、slotを増やしている。
  return(new("schwartz2f",
             call = call,
             s0 = unname(s0),
             delta0 = unname(delta0),
             mu = unname(mu),
             sigmaS = unname(sigmaS),
             kappaE = unname(kappa),
             alpha = unname(alpha),
             sigmaE = unname(sigmaE),
             rhoSE = unname(rho)))
}

### <---------------------------------------------------------------------->

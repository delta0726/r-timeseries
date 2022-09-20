# ***********************************************************************************************
# Title   : RとStan出始める心理学のための時系列分析入門
# Chapter : 3 時系列の回帰分析
# Theme   : 系列相関
# Date    : 2022/09/21
# Page    : P70 - P76
# URL     : https://github.com/komorimasashi/time_series_book
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 ダービン-ワトソン検定
# 2 一般化最小二乗法


# 0 準備 -------------------------------------------------------------------------

# ライブラリ
library(lmtest)
library(nlme)


# 1 ダービン-ワトソン検定 ----------------------------------------------------------

# AR(1)過程のシミュレーション
AR1_model <- list(order = c(1, 0, 0), ar = 0.9)
AR1_sm <- arima.sim(n = 100, model = AR1_model, sd = 1)
fit <- lm(AR1_sm ~1)

# ダービン-ワトソン検定
fit %>% dwtest()


# 2 一般化最小二乗法 --------------------------------------------------------------

# ＜ポイント＞
# - 一般化最小二乗法は残差系列に対して明示的にモデリングする


# 乱数シード
set.seed(1234)

# データ作成
n <- 50
x <- seq(50)

# AR(1)過程のシミュレーション
AR1_model <- list(order = c(1, 0, 0), ar = 0.9)
AR1_sm <- arima.sim(n = n, model = AR1_model, sd = 1)
ts.plot(AR1_sm)
# 普通にOLSで回帰分析を行います
fit.lm <- lm(AR1_sm ~ x)
# 残差の自己相関
fit.lm$residuals %>% acf()

# ダービン-ワトソン検定
fit.lm %>% dwtest()

# 一般化最小二乗法（GLS）で回帰分析
# correlationパラメータには残差の分散共分散行列のモデルを指定できる
# corAR1()は残差がAR(1)に従うという意味
fit.gls <- gls(AR1_sm ~ x, correlation = corAR1())
# fit.gls <- gls(AR1_sm ~ x, correlation = corARMA(p=1, q=0))　と書いてもよい

# サマリー
fit.gls %>% summary()

# プロット作成
# --- OLSとGLSの回帰直線の違いを見る
plot(x, AR1_sm, pch=20)
lines(x, predict(fit.lm), col=1, lty=1)
lines(x, predict(fit.gls), col=2, lty=1)
legend("topleft", legend=c("DATA","OLS", "GLS"), col=c(1,1,2),pch=c(20,NA,NA),lty=c(0,1,1))

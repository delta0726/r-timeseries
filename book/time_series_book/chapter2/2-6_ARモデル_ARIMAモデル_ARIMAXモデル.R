# ***********************************************************************************************
# Title   : RとStan出始める心理学のための時系列分析入門
# Chapter : 2 時系列分析の基本操作
# Theme   : ARモデル / ARIMAモデル / ARIMAXモデル
# Date    : 2022/09/18
# Page    : P53 - P60
# URL     : https://github.com/komorimasashi/time_series_book
# ***********************************************************************************************


# ＜概要＞
# - 時系列データは｢トレンド｣｢季節性｣｢不規則変動｣などに分解することができる
# - 現在ではLOESSでを使うことで、移動平均よりも頑健にトレンド成分を抽出する
#   --- 局所カーネル平滑化手法を使って外れ値の影響を受けにくくしている


# ＜目次＞
# 0 準備
# 1 ARモデル
# 2 ARIMAモデル
# 3 ARIMAXモデル


# 0 準備 ---------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(forecast)
library(crayon)


# 1 ARモデル -------------------------------------------------------

# ARモデル
# --- φ=0.1： 自己相関が弱い
# --- φ=0.9： 自己相関が強い
model.small <- list(order = c(1, 0, 0), ar = 0.1, sd = 0.1)
model.large <- list(order = c(1, 0, 0), ar = 0.9, sd = 0.1)

# シミュレーションデータ生成
AR1.small <- arima.sim(model = model.small, n = 500)
AR1.large <- arima.sim(model = model.large, n = 500)

# 描画のセットアップ
par(mfrow = c(2, 2))
ylm <- c(min(AR1.small, AR1.large), max(AR1.small, AR1.large))

# 時系列の描画
AR1.small %>% plot.ts(ylim = ylm, ylab = expression(italic(y)[italic(t)]), main = expression(paste(phi, " = 0.1")))
AR1.large %>% plot.ts(ylim = ylm, ylab = expression(italic(y)[italic(t)]), main = expression(paste(phi, " = 0.9")))

# 自己相関関数
AR1.small %>% acf(main = "")
AR1.large %>% acf(main = "")


# 2 ARIMAモデル---------------------------------------------------------

# データ確認
# --- アスワンでのナイル川の流量
data <- Nile %>% ts(start = 1871)
data %>% plot(main = "Flow of the river Nile")

# ARIMA
model <- data %>% auto.arima(ic = "aic", stepwise = T, trace = T)

# サマリー
model %>% summary()

# 予測値と信頼区間のプロット（20時点）
model %>%
  forecast(level = c(50, 95), h = 20) %>%
  plot(shadecols = c("gray", "darkgray"))


# 3 ARIMAXモデル ----------------------------------------------------

# 外生変数の作成
# --- ダムなし: 1871-1901(31年間)
# --- ダムあり: 1902-1970(69年間)
x <- ts(c(rep(0, 31), rep(1, 69)))

# モデル構築
# --- (xregに外生変数を入れる)
modelx <- data %>% auto.arima(xreg = x, ic = "aic", stepwise = T, trace = T)

# サマリー
modelx %>% summary()

# 予測用データ
# --- ダムが取り壊された場合の将来予測
# --- ダムが取り壊されたと考える（20年分）
x_pred <- rep(0, 20)

#
modelx %>%
  forecast(level = c(50, 95), h = 20, xreg = x_pred) %>%
  plot(shadecols = c("gray", "darkgray"))

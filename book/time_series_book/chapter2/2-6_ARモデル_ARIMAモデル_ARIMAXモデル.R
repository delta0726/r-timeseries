# ***********************************************************************************************
# Title   : RとStan出始める心理学のための時系列分析入門
# Chapter : 2 時系列分析の基本操作
# Theme   : ARモデル / ARIMAモデル / ARIMAXモデル
# Date    : 2022/09/18
# Page    : P53 - P60
# URL     : https://github.com/komorimasashi/time_series_book
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 ARモデル
# 2 MAモデル
# 3 ARIMAモデル
# 4 ARIMAXモデル


# 0 準備 ---------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(forecast)
library(crayon)


# 1 ARモデル -------------------------------------------------------

# ＜ポイント＞
# - ARモデルは過去の系列推移が現在の値に影響するモデル（自己回帰モデル）


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


# 2 MAモデル------------------------------------------------------------

# ＜ポイント＞
# - 移動平均(MA)モデルは現在の誤差項に加えて、以前の誤差項も現在の値に影響するモデル
#   --- θは過去の誤差項εがどの程度影響を及ぼすかを示す係数
#   --- チラシ配布の効果（配布直後に売上が上昇、徐々に減衰しながらも売上が高い状態が続く）
#   --- 単独で当モデルが用いられることは少ない


# 3 ARIMAモデル---------------------------------------------------------

# ＜ポイント＞
# - ARMAモデルはARモデルとMAモデルを組み合わせたもの（自己回帰移動平均モデル）
# - ARIMAモデルはARMAモデルを差分系列に対して適用したもの（自己回帰和分移動平均モデル）
#   --- ARIMAモデルはボックス・ジェンキンス法と呼ばれることもある


# データ確認
# --- アスワンでのナイル川の流量
data <- Nile %>% ts(start = 1871)
data %>% plot(main = "Flow of the river Nile")

# モデル構築
# --- 自動でARIMAモデルの最良モデルを見つけてくれる
model <- data %>% auto.arima(ic = "aic", stepwise = T, trace = T)

# サマリー
model %>% summary()

# プロット
# --- 予測値と信頼区間のプロット
# --- 予測値は具体的な系列推移を示すのではなく、平均値の水準を示すのみ
model %>%
  forecast(level = c(50, 95), h = 20) %>%
  plot(shadecols = c("gray", "darkgray"))


# 4 ARIMAXモデル ----------------------------------------------------

# ＜ポイント＞
# - ARIMAXモデルはARIMAモデルに外生変数を加えて介入操作の影響を分析するモデル


# 外生変数の作成
# --- ダムなし: 0 (1871-1901 31年間)
# --- ダムあり: 1 (1902-1970 69年間)
x <- c(rep(0, 31), rep(1, 69)) %>% ts()

# データ確認
data %>% length()
x %>% length()

# モデル構築
# --- xregに外生変数を入れる
modelx <- data %>% auto.arima(xreg = x, ic = "aic", stepwise = T, trace = T)

# サマリー
modelx %>% summary()

# 予測用データ
# --- 0を入力（ダムが取り壊された場合の将来予測）
x_pred <- rep(0, 20)

# プロット
# --- ダムがなかった期間の水準を予測している
modelx %>%
  forecast(level = c(50, 95), h = 20, xreg = x_pred) %>%
  plot(shadecols = c("gray", "darkgray"))

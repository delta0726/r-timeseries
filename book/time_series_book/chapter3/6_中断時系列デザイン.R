# ***********************************************************************************************
# Title   : RとStan出始める心理学のための時系列分析入門
# Chapter : 3 時系列の回帰分析
# Theme   : 中断時系列デザイン
# Date    : 2022/09/21
# Page    : P100 - P112
# URL     : https://github.com/komorimasashi/time_series_book
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 線形回帰モデル
# 2 一般線形モデル（線形回帰）


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tsModel)
library(lmtest)


# データロード
data <- read_csv("data/sicily.csv")

# データ確認
data %>% glimpse()


# 1 線形回帰モデル --------------------------------------------------------------

# 一般線形モデル（線形回帰）介入×時間

# モデル構築
# --- 傾きの変化を考慮したモデル
fit1 <- glm(aces ~ smokban * time, family = gaussian, data)

# モデル構築
# --- 傾きの変化を考慮しないモデル
# fit1 <- glm(aces ~ smokban + time , family=gaussian, data)

# サマリー
fit1 %>% summary()

# 予測
pred1 <- fit1 %>% predict()

# プロット
data$aces %>% plot(ylim = c(0, 1000), xlab = "Time", ylab = "ACEs")
points(pred1, col = 2, type = "l")

# 自己相関
fit1$residuals %>% acf()


# 2 一般線形モデル（線形回帰）-------------------------------------------------

# 介入×時間＋季節成分

# harmonic(time,2,12)の2番目の引数で調和数，3番目の引数で周期を指定します
fit2 <- glm(aces ~ smokban * time + harmonic(time, 2, 12), family = gaussian, data)

# サマリー
fit2 %>% summary()

# 予測
pred2 <- fit2 %>% predict()

# プロット
data$aces %>% plot(ylim = c(0, 1000))
pred2 %>% points(col = 2, type = "l")

# ダービン-ワトソン検定
fit2 %>% dwtest()


# 2 一般線形モデル（ポアソン回帰）-------------------------------------------------


# 介入×時間 ------------------------------------------------

# モデル構築
fit3 <- glm(aces ~ smokban * time, family = poisson(link = "log"), data)

# サマリー
fit3 %>% summary()

# 予測
pred3 <- fit3 %>% predict()

# プロット
data$aces %>% plot(ylim = c(0, 1000))
pred3 %>% exp() %>% points(col = 2, type = "l")

# 自己相関
fit3$residuals %>% acf()


# 介入×時間＋季節成分 ---------------------------------------

# モデル構築
fit4 <- glm(aces ~ smokban * time + harmonic(time, 2, 12), family = poisson(link = "log"), data)

# サマリー
fit4 %>% summary()

# 予測
pred4 <- fit4 %>% predict()

# プロット
data$aces %>% plot(ylim = c(0, 1000))
pred4 %>% exp() %>% points(col = 2, type = "l")

# 自己相関
fit4$residuals %>% acf()


# オフセット項+介入×時間＋季節成分 ---------------------------------------

# モデル構築
# --- 人口stdpopをオフセット項として投入している
fit5 <- glm(aces ~ offset(log(stdpop)) + smokban * time + harmonic(time, 2, 12),
            family = poisson(link = "log"), data)

# サマリー
fit5 %>% summary()

# 予測
pred5 <- fit5 %>% predict()

# プロット
data$aces %>% plot(ylim = c(0, 1000))
pred5 %>% exp() %>% points(col = 2, type = "l")

# 自己相関
fit5$residuals %>% acf()
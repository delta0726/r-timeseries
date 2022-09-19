# ***********************************************************************************************
# Title   : RとStan出始める心理学のための時系列分析入門
# Chapter : 3 時系列の回帰分析
# Theme   : 時系列同士の回帰分析
# Date    : 2022/09/19
# Page    : P83 - P91
# URL     : https://github.com/komorimasashi/time_series_book
# ***********************************************************************************************


# ＜概要＞
# - 非定常性の時系列の問題、系列相関の問題を考慮して回帰分析を行う


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 ADF検定
# 3 モデル構築：単純なOLS回帰
# 4 モデル構築：季節成分を考慮したOLS回帰
# 5 モデル構築：季節成分を考慮したGLS回帰


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(MARSS)
library(tseries)
library(tsModel)
library(lmtest)
library(broom)
library(nlme)
library(gridExtra)
library(CADFtest)

# データロード
data(lakeWAplankton)


# 1 データ準備 ----------------------------------------------------------------

# データ加工
plankdf <-
  lakeWAplanktonTrans %>%
    as_tibble() %>%
    filter(between(Year, 1980, 1990)) %>%
    mutate(Time = as.POSIXct(paste(Year, Month, 1), format = "%Y %m %d"))

# プロット作成
# --- ミジンコの数
p1 <-
  plankdf %>%
    ggplot(aes(x = Time, y = Cyclops)) +
    geom_line() +
    ggtitle("Cyclops")

# プロット作成
# --- 気温
p2 <-
  plankdf %>%
    ggplot(aes(x = Time, y = Temp)) +
    geom_line() +
    ggtitle("Temp")

# プロット比較
grid.arrange(p1, p2)


# 2 ADF検定 ------------------------------------------------------------------

# ＜ポイント＞
# - ADF検定は帰無仮説が｢単位根を持つ｣で、対立仮設の｢単位根を持たない｣の棄却を目指す

# ADF検定
plankdf$Temp %>% CADFtest(type = 'trend', max.lag.y = 4, criterion = 'AIC')
plankdf$Cyclops %>% CADFtest(type = 'trend', max.lag.y = 4, criterion = 'AIC')


# 3 モデル構築：単純なOLS回帰-----------------------------------------------

# ＜ポイント＞
# - 単純に回帰分析をして残差の自己相関を見てみる
#   --- ACFから残差の自己相関から強い周期性成分があることが確認できる


# モデル構築
# --- 季節成分を考慮しない単純なOLS回帰
fit1 <- lm(Cyclops ~ Temp, plankdf)

# サマリー
fit1 %>% summary()

# ACF
# --- 強い周期性成分がある
fit1$residuals %>% acf()


# 4 モデル構築：季節成分を考慮したOLS回帰-------------------------------------

# ＜ポイント＞
# - 季節性分を係数に導入するためフーリエ展開を用いる


# フーリエ展開
plankdf$Month %>% harmonic(nfreq = 2, period = 12) %>% head()

# モデル構築
# --- フーリエ展開による季節成分を考慮したOLS回帰
fit2 <- lm(Cyclops ~ Temp + harmonic(Month, 2, 12), plankdf)

# サマリー
fit2 %>% summary()

# ACF
# --- 周期性成分がほぼ消えた
fit2$residuals %>% acf()

# ダービン-ワトソン検定
# --- 系列相関は相変わらず高い
fit2 %>% dwtest()


# 5 モデル構築：季節成分を考慮したGLS回帰 ------------------------------------

# モデル構築
# --- OLS回帰ではなくGLSを用いる
# --- 残差の共分散構造をAR(1)モデルとする
fit3 <- gls(Cyclops ~ Temp + harmonic(Month, 2, 12), correlation = corAR1(), data = plankdf)

# サマリー
fit3 %>% summary()

# ACF
fit3$residuals %>% acf()

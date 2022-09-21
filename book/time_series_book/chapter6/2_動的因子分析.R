# ***********************************************************************************************
# Title   : RとStan出始める心理学のための時系列分析入門
# Chapter : 6 多変量時系列データの要約
# Theme   : 動的因子分析
# Date    : 2022/09/22
# Page    : P219 - P223
# URL     : https://github.com/komorimasashi/time_series_book
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ加工
# 2 DFAの実行


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(MARSS)


# データロード
data(lakeWAplankton)


# 1 データ加工 ----------------------------------------------------------------------

# データ作成
# --- 1980年から1990年までのプランクトンの量のデータを抜き出す
# --- 時刻が列に対応するように転置する
plankdat <- lakeWAplanktonTrans
dat <- plankdat[1980 <= plankdat[, "Year"] & plankdat[, "Year"] <= 1990,
                c("Cryptomonas", "Diatoms", "Cyclops", "Unicells", "Epischura")]
dat <- t(dat)


# 2 DFAの実行 ----------------------------------------------------------------------

# モデル構築
# --- RおよびQのモデルを設定する
model.list = list(m = 3, R = "diagonal and unequal", Q = "identity")
fit <- MARSS(dat, model = model.list, z.score = TRUE, form = "dfa", control = list(maxit = 3000))

# サマリー
# --- 結果の概要（Zは因子負荷量，Rは観測誤差の推定値が示される）
# --- 因子得点fはfit$statesに入っている
fit %>% summary()

# プロット作成
# --- 推定された状態f_tのグラフ
# --- 観測値と観測値の予測平均値とCIのグラフ
fit %>% plot(plot.type = "xtT")
fit %>% plot(plot.type = "fitted.ytT")

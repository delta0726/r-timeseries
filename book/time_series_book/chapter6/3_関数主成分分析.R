# ***********************************************************************************************
# Title   : RとStan出始める心理学のための時系列分析入門
# Chapter : 6 多変量時系列データの要約
# Theme   : 関数主成分分析
# Date    : 2022/09/26
# Page    : P223 - P229
# URL     : https://github.com/komorimasashi/time_series_book
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 基底関数の作成(フーリエ級数)
# 3 基底関数の作成(B-スプライン関数)
# 4 関数主成分分析の実行


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(fda)

# データロード
# --- 膝関節データの読み込み
data(gait)


# 1 データ確認 -----------------------------------------------------------------

# データ確認
gait %>% head()
gait %>% glimpse()
gait[,,2] %>% head()

# プロット作成
# --- 元データのプロット(20時点，39人)
gait[,,2] %>% matplot(type = "l", xlab = "time", ylab = "angle")


# 2 基底関数の作成(フーリエ級数) ---------------------------------------------------

# 8つの基底関数を作成
basis.f <- create.fourier.basis(c(0, 1), nbasis = 8)

# 基底関数の当てはめ
gait.ft <- smooth.basis(dimnames(gait)[[1]], gait[,,2], basis.f)

# 描画
gait.ft %>% plot(xlab = "time", ylab = "angle")

# 平均関数
gait.ft$fd %>% mean.fd() %>% lines(lwd = 3, col = 1)


# 3 基底関数の作成(B-スプライン関数) ------------------------------------------------

# 8つの基底関数を作成
basis.b <- create.bspline.basis(c(0,1), nbasis = 8, norder = 4)

# 基底関数の当てはめ
gait.bs <- smooth.basis(dimnames(gait)[[1]], gait[,,2], basis.b)

# 描画
gait.bs %>% plot(xlab = "time", ylab = "angle")

# 平均関数
gait.bs$fd %>% mean.fd() %>% lines(lwd = 3, col = 1)


# 4 関数主成分分析の実行 ------------------------------------------------------------

# まず基底関数と同じ数の主成分を求める
gait.pca <- gait.ft$fd %>% pca.fd(nharm = 8)

# 累積寄与率
gait.pca$varprop %>%
  cumsum() %>%
  plot(type = "o", xlab = 'component', ylab = 'contribution')

# 固有関数
gait.pca$harmonics[1:3] %>% plot()

# 角度パターンの各種成分軸に沿った変化
gait.pca %>% plot.pca.fd(cex.main = 1)

# 主成分得点
# --- PC1 - PC2
gait.pca$scores[, c(1, 2)] %>% plot(xlab = "FPC Score 1", ylab = "FPC Score 2")
gait.pca$scores[, c(1, 2)] %>% text(labels = dimnames(gait)[[2]], cex = 1)

# 主成分得点
# --- PC1 - PC3
gait.pca$scores[,c(1,3)] %>% plot(xlab = "FPC Score 1", ylab = "FPC Score 3")
gait.pca$scores[,c(1,3)] %>% text(labels = dimnames(gait)[[2]], cex = 1)


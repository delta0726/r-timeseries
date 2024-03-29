# ***********************************************************************************************
# Title   : RとStan出始める心理学のための時系列分析入門
# Chapter : 3 時系列の回帰分析
# Theme   : 潜在成長曲線モデル
# Date    : 2022/09/21
# Page    : P91 - P100
# URL     : https://github.com/komorimasashi/time_series_book
# ***********************************************************************************************


# ＜概要＞
# - パネルデータとは複数人を対象にして複数時点にわたって継続的に調査されたデータ
#   --- 継続対象が経時的に変化する場合はパネルデータとは呼ばない
#   --- パネルデータ分析ではマルチレベル分析が用いられる

# - 潜在成長曲線モデルとは時間を連続量として説明変数に用いるモデルを指す


# ＜目次＞
# 0 準備
# 1 データ作成
# 2 ランダム切片モデル
# 3 潜在成長モデル


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(nlme)

# データ確認
BodyWeight %>% as_tibble()


# 1 データ作成 ------------------------------------------------------------------

# データ加工
# --- 等間隔でないデータを削除(44)
# --- 週に変換
dat <-
  BodyWeight %>%
    filter(Diet %in% c(1, 2)) %>%
    filter(Time != 44) %>%
    mutate(Time = (Time - 1) / 7) %>%
    as_tibble()


# 2 ランダム切片モデル -----------------------------------------------------------

# ランダム切片モデル
lme(weight ~ 1, random = ~1 | Rat, data = dat)
# library(lme4) # {lme4}で書く場合
# lmer(weight ~ 1 + (1 | Rat), data = dat)


# 3 潜在成長モデル -------------------------------------------------------------

# 残差の系列相関を考慮しない潜在成長曲線モデル
fit1 <- lme(weight ~ Time * Diet, random = ~Time | Rat, data = dat)
fit1 %>% ACF() %>% plot(alpha = 0.01)

# 残差の系列相関を考慮した潜在成長曲線モデル
fit2 <- lme(weight ~ Time * Diet, random = ~Time | Rat,
            correlation = corAR1(form = ~Time | Rat), data = dat)

fit2 %>% summary()

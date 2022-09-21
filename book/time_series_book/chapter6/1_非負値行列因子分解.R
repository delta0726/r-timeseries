# ***********************************************************************************************
# Title   : RとStan出始める心理学のための時系列分析入門
# Chapter : 6 多変量時系列データの要約
# Theme   : 非負値行列因子分解
# Date    : 2022/09/22
# Page    : P214 - P218
# URL     : https://github.com/komorimasashi/time_series_book
# ***********************************************************************************************


# ＜概要＞
# -


# ＜目次＞
# 0 準備
# 1 NMFの実行


# 0 準備 ----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(NMF)


# データロード
df <- read_csv("data/abe_facial_mov.csv")

# データ確認
df %>% glimpse()

# プロット
df %>%
  gather(key=AU, value = SCORE, colnames(df)[-1]) %>%
  ggplot(aes(x = FRAME, y = SCORE, color = AU)) +
  geom_line()


# 1 NMFの実行 ----------------------------------------------------------------

# MMF
res <-
  df %>%
    select(-FRAME) %>%
    nmf(, rank = 2, seed = 1234, .options = "t")

# プロット作成
# --- 'Rowv=NA'とすることで時系列順に表示されます
res %>% basismap(Rowv=NA)
res %>% coefmap()

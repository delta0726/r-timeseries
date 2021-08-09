# ***************************************************************************************
# Library   : timetk
# Function  : tk_time_series_cv_plan
# Created on: 2021/8/9
# URL       : https://business-science.github.io/timetk/reference/tk_time_series_cv_plan.html
# ***************************************************************************************


# ＜概要＞
# - rsample::rolling_originまたはtimetk::time_series_cvクラスのいずれかの時系列リサンプルをラベル付けする


# ＜構文＞
# tk_time_series_cv_plan(.data)


# ＜目次＞
# 0 準備
# 1 時系列リサンプリング
# 2 データのラベル付け


# 0 準備 -----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(rsample)
library(timetk)


# データ準備
FB_tbl <-
  FANG %>%
    filter(symbol == "FB") %>%
    select(symbol, date, adjusted)


# 1 時系列リサンプリング -----------------------------------------------------------------

# リサンプルデータの作成
resample_spec <-
  FB_tbl %>%
    time_series_cv(initial = 150,
                   assess = 50,
                   skip = 50,
                   cumulative = FALSE,
                   lag = 30,
                   slice_limit = n())

# 確認
resample_spec %>% print()
resample_spec$splits[[1]] %>% training()
resample_spec$splits[[1]] %>% testing()


# 2 データのラベル付け ------------------------------------------------------------------------

# CVプランの確認
resample_spec %>%
  tk_time_series_cv_plan()

# データ構造の確認
resample_spec %>%
  tk_time_series_cv_plan() %>%
  group_by(.id, .key) %>%
  tally() %>%
  print(n = nrow(.))



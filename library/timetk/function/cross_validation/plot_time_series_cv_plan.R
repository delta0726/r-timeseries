# ***************************************************************************************
# Library   : timetk
# Function  : plot_time_series_cv_plan
# Created on: 2021/8/
# URL       : https://business-science.github.io/timetk/reference/plot_time_series_cv_plan.html
# ***************************************************************************************


# ＜概要＞
# - 時系列リサンプリングを可視化する
#   --- rsample::rolling_originまたはtimetk::time_series_cvクラス


# ＜構文＞
# plot_time_series_cv_plan(
#   .data,
#   .date_var,
#   .value,
#   ...,
#   .smooth = FALSE,
#   .title = "Time Series Cross Validation Plan"
# )


# ＜使用例＞
# 0 準備
# 1 時系列データ分割
# 2 CVプランの確認


# 0 準備 ------------------------------------------------------------------------------------

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

# データ確認
FB_tbl %>% print()


# 1 時系列データ分割 -----------------------------------------------------------------------

# データ分割
resample_spec <- 
  FB_tbl %>% 
  time_series_cv(initial = "1 year",
                 assess  = "6 weeks",
                 skip    = "3 months",
                 lag     = "1 month",
                 cumulative  = FALSE,
                 slice_limit = 4)

# 確認
resample_spec %>% print()
resample_spec$splits[[1]] %>% training()
resample_spec$splits[[1]] %>% testing()


# 2 CVプランの確認 ------------------------------------------------------------------------

# データ構造の確認
resample_spec %>% 
  tk_time_series_cv_plan() %>% 
  group_by(.id, .key) %>% 
  tally() %>% 
  print(n = nrow(.))

# 期間の確認
# --- 検証期間はリサンプリングごとに重複しない
# --- 下(Slice5)から古い期間で表示されている
resample_spec %>% 
  tk_time_series_cv_plan() %>% 
  group_split(.id, .key) %>% 
  map(tk_summary_diagnostics, .date_var = date) %>% 
  map(select, 1:4) %>% 
  bind_rows()

# プロット確認
# --- 検証期間はリサンプリングごとに重複しないことが確認できる
resample_spec %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(.date_var = date, 
                           .value    = adjusted, 
                           .facet_ncol = 1, 
                           .line_alpha = 0.5, 
                           .interactive = FALSE)



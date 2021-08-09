# ***************************************************************************************
# Library   : timetk
# Function  : time_series_cv
# Created on: 2021/8/9
# URL       : https://business-science.github.io/timetk/reference/time_series_cv.html
# ***************************************************************************************



# ＜概要＞
# - 時系列データでクロスバリデーション用のリサンプリングデータを作成する


# ＜構文＞
# time_series_cv(
# data,
# date_var = NULL,
# initial = 5,
# assess = 1,
# skip = 1,
# lag = 0,
# cumulative = FALSE,
# slice_limit = n(),
# ...
# )


# ＜使用例＞
# 0 準備
# 1 単一系列のリサンプリング
# 2 パネルデータの時系列リサンプリング


# 0 準備 ------------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)
library(rsample)


# データ準備
# --- 単一系列
m750 <- 
  m4_monthly %>% 
    filter(id == "M750")

# データ準備
# --- 複数系列
walmart_sales_weekly %>% print()
walmart_sales_weekly %>% group_by(id) %>% tally()


# 1 単一系列のリサンプリング -----------------------------------------------------------------

# リサンプリング
resample_spec <- 
  m750 %>% 
    time_series_cv(initial     = "6 years", 
                   assess      = "24 months", 
                   skip        = "24 months", 
                   cumulative  = FALSE, 
                   slice_limit = 3)

# データ確認
resample_spec %>% print()
resample_spec$splits[[1]] %>% training()
resample_spec$splits[[1]] %>% testing()

# レコード数の確認
resample_spec %>% 
  tk_time_series_cv_plan() %>% 
  group_by(.id, .key) %>% 
  tally()

# プロット確認
resample_spec %>%
  plot_time_series_cv_plan(date, value, .interactive = FALSE)


# 2 パネルデータの時系列リサンプリング ------------------------------------------------------

# ＜ポイント＞
# - パネルデータの場合にパネルキーを明示的に指定する箇所はない
# - データ分割は日付を基準に行っており重複日付は同様に処理される
#   --- 結果として、パネルキーが異なる系列もうまく処理されている


# リサンプリング
walmart_tscv <- 
  walmart_sales_weekly %>%
    time_series_cv(date_var    = Date, 
                   initial     = "12 months", 
                   assess      = "3 months", 
                   skip        = "3 months", 
                   slice_limit = 4)

# データ確認
walmart_tscv %>% print()
walmart_tscv$splits[[1]] %>% training()
walmart_tscv$splits[[1]] %>% testing()

# レコード数の確認
walmart_tscv %>% 
  tk_time_series_cv_plan() %>% 
  group_by(.id, .key, id) %>% 
  tally() %>% 
  filter(.id == "Slice1") %>% 
  print(n = nrow(.))

# プロット確認
walmart_tscv %>%
  plot_time_series_cv_plan(Date, Weekly_Sales, .interactive = FALSE)


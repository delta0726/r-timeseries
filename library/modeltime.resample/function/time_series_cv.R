# ***************************************************************************************
# Library   : modeltime.resample
# Function  : time_series_cv
# Created on: 2021/8/8
# URL       : https://business-science.github.io/modeltime.resample/reference/time_series_cv.html
# ***************************************************************************************


# ＜概要＞
# - 時系列データでリサンプリングデータを作成する
#   --- 開始時点を複数ポイント設定してAnalysis/Assessmentのデータセットを複数生成する（｢2 可視化｣を参照）
#   --- クロスセクションのリサンプリングは同じデータを使わないが、時系列では使わざるをえない


# ＜構文＞
# - 以下のリサンプリングツールは{timetk}からインポートされている
#   --- timetk::time_series_cv()
#   --- timetk::time_series_split()
#   --- timetk::plot_time_series_cv_plan()
#   --- timetk::tk_time_series_cv_plan()


# ＜使用例＞
# 0 準備
# 1 時系列リサンプリング
# 2 可視化


# 0 準備 --------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)
library(modeltime)
library(modeltime.resample)


# データ確認
# --- 1系列データ
m750 %>% print()


# 1 時系列リサンプリング ------------------------------------------------------------------

# Generate Time Series Resamples
resamples_tscv <-
  m750 %>%
    time_series_cv(date_var    = date,
                   assess      = "2 years",
                   initial     = "5 years",
                   skip        = "2 years",
                   slice_limit = 4)


# データ確認
resamples_tscv %>% print()

# 要素の確認
# --- 複数地点を基準に同じ分析期間でデータを分割している
resamples_tscv$splits[1]
resamples_tscv$splits[2]
resamples_tscv$splits[3]
resamples_tscv$splits[4]


# 2 可視化 --------------------------------------------------------------------------------

# プロット作成
resamples_tscv %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(.date_var = date,
                             .value = value,
                             .facet_ncol  = 2,
                             .interactive = FALSE)

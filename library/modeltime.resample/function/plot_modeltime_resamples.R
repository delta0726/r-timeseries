# ***************************************************************************************
# Library   : modeltime.resample
# Function  : plot_modeltime_resamples
# Created on: 2021/8/8
# URL       : https://business-science.github.io/modeltime.resample/reference/plot_modeltime_resamples.html
# ***************************************************************************************


# ＜概要＞
# - クロスバリデーションによる予測精度を可視化する


# ＜構文＞
# plot_modeltime_resamples(
#   .data,
#   .metric_set = default_forecast_accuracy_metric_set(),
#   .summary_fn = mean,
#   ...,
#   .facet_ncol = NULL,
#   .facet_scales = "free_x",
#   .point_show = TRUE,
#   .point_size = 1,
#   .point_shape = 16,
#   .point_alpha = 1,
#   .summary_line_show = TRUE,
#   .summary_line_size = 0.5,
#   .summary_line_type = 1,
#   .summary_line_alpha = 1,
#   .x_intercept = NULL,
#   .x_intercept_color = "red",
#   .x_intercept_size = 0.5,
#   .legend_show = TRUE,
#   .legend_max_width = 40,
#   .title = "Resample Accuracy Plot",
#   .x_lab = "",
#   .y_lab = "",
#   .color_lab = "Legend",
#   .interactive = TRUE
# )


# ＜使用例＞
# 0 準備
# 1 クロスバリデーション結果の可視化


# 0 準備 --------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)
library(modeltime)
library(modeltime.resample)


# データ確認
# --- モデルテーブルでCVを実行済
m750_training_resamples_fitted

# 要素確認
m750_training_resamples_fitted$.resample_results[1]


# 1 クロスバリデーション結果の可視化 ---------------------------------------------------------

# プロット作成
m750_training_resamples_fitted %>%
    plot_modeltime_resamples(.interactive = FALSE)

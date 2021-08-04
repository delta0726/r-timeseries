# ***************************************************************************************
# Library   : modeltime
# Function  : summarize_accuracy_metrics
# Created on: 2021/8/5
# URL       : https://business-science.github.io/modeltime/reference/summarize_accuracy_metrics.html
# ***************************************************************************************


# ＜概要＞
# - modeltime_accuracy()で使用されている内部関数


# ＜構文＞
# summarize_accuracy_metrics(data, truth, estimate, metric_set)


# ＜使用例＞
# 0 準備
# 1 メトリックの計算


# 0 準備 ------------------------------------------------------------------------------------

# ライブラリ
library(tibble)
library(dplyr)


# データ作成
predictions_tbl <-
  tibble(group = c("model 1", "model 1", "model 1", "model 2", "model 2", "model 2"),
         truth = c(1, 2, 3, 1, 2, 3),
         estimate = c(1.2, 2.0, 2.5, 0.9, 1.9, 3.3))


# 1 メトリックの計算 ---------------------------------------------------------------------------

# メトリックの計算
# --- メトリックセットに基づく
predictions_tbl %>%
    group_by(group) %>%
    summarize_accuracy_metrics(truth = truth, estimate = estimate,
                               metric_set = default_forecast_accuracy_metric_set())

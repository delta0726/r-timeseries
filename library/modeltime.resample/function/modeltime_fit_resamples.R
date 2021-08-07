# ***************************************************************************************
# Library   : modeltime.resample
# Function  : modeltime_fit_resamples
# Created on: 2021/8/8
# URL       : https://business-science.github.io/modeltime.resample/reference/modeltime_fit_resamples.html
# ***************************************************************************************


# ＜概要＞
# - モデルテーブルをリサンプリングデータを用いて学習する


# ＜構文＞
# modeltime_fit_resamples(object, resamples, control = control_resamples())


# ＜引数＞
# object    ：モデルテーブル
# resamples ：リサンプリングデータセット


# ＜詳細設定＞
# tune::control_resamples(
#   verbose = FALSE,
#   allow_par = TRUE,
#   extract = NULL,
#   save_pred = FALSE,
#   pkgs = NULL,
#   save_workflow = FALSE,
#   event_level = "first",
#   parallel_over = NULL
# )


# ＜使用例＞
# 0 準備
# 1 リサンプリングデータの作成
# 2 時系列クロスバリデーション


# 0 準備 --------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)
library(modeltime)
library(modeltime.resample)


# データ確認
# --- rsplitオブジェクト
m750_splits %>% print()

# 期間確認
# --- Holdout法で分割（時系列の場合は期間で分ける）
m750_splits %>% training() %>% tk_summary_diagnostics() %>% select(1:4)
m750_splits %>% testing() %>% tk_summary_diagnostics() %>% select(1:4)


# 1 リサンプリングデータの作成 --------------------------------------------------------------

# ＜ポイント＞
# - リサンプリングデータの各Foldデータを作成する


# リサンプリングデータの作成
resamples_tscv <-
  m750_splits %>%
    training() %>%
    time_series_cv(date_var    = date,
                   assess      = "2 years",
                   initial     = "5 years",
                   skip        = "2 years",
                   slice_limit = 1)

# 確認
resamples_tscv %>% print()

# 要素確認
resamples_tscv$splits[[1]] %>% analysis() %>% tk_summary_diagnostics() %>% select(1:4)
resamples_tscv$splits[[1]] %>% assessment() %>% tk_summary_diagnostics() %>% select(1:4)


# 2 時系列クロスバリデーション ---------------------------------------------------------------

# ＜ポイント＞
# - リサンプリングデータの各Foldデータを作成する

# 学習
m750_models_resample <-
  m750_models %>%
    modeltime_fit_resamples(resamples = resamples_tscv,
                            control   = control_resamples(verbose = TRUE))

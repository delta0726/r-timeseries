# ****************************************************************************
# Title       : BUSINESS SCIENCE UNIVERSITY
# Course      : DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# Module      : NEW FEATURES - MODELTIME 0.1.0 （7-8 to 7-9）
# Created by  : Owner
# Last Update : 2021/6/27
# URL         : https://university.business-science.io/
# ****************************************************************************


# ＜ゴール＞
# MODELTIME 0.1.0のフレキシビリティを確認する
# MODELTIMEを用いた時系列データ分析を確認する


# ＜目的＞
# - Expedited Forecasting - Skip Calibrating / Refitting
# - Working with In-Sample & Out-of-Sample Data
# - NEW Residual Diagnositics


# ＜目次＞
# 0 準備
# 1 予測力を確認するためのテクニック
# 2 柔軟にCalibrationを行う
# 3 残差分析


# 0 準備 ------------------------------------------------------------------------------

# * ライブラリ ---------------------------------------------------------

# Time Series ML
library(tidymodels)
library(modeltime)

# Core
library(tidyverse)
library(timetk)
library(lubridate)


# * データ準備 --------------------------------------------------------

# ディレクトリ設定
setwd("course/advanced_timeseries")

# データ準備
feature_engineering_artifacts_list <-
  read_rds("00_models/feature_engineering_artifacts_list.rds")

# データ取得
data_prepared_tbl <- feature_engineering_artifacts_list$data$data_prepared_tbl
forecast_tbl      <- feature_engineering_artifacts_list$data$forecast_tbl

# データ確認
data_prepared_tbl %>% glimpse()
forecast_tbl %>% glimpse()

# レシピ取得
recipe_spec_1_spline <- feature_engineering_artifacts_list$recipes$recipe_spec_1
recipe_spec_2_lag    <- feature_engineering_artifacts_list$recipes$recipe_spec_2

# レシピ確認
recipe_spec_1_spline %>% tidy()
recipe_spec_2_lag %>% tidy()


# TRAIN / TEST ------------------------------------------------------------

# データ分割
splits <-
  data_prepared_tbl %>%
    time_series_split(assess = "8 weeks", cumulative = TRUE)

# 確認
splits %>% print()
splits %>% training() %>% tk_index() %>% tk_get_timeseries_summary() %>% select(1:4)
splits %>% testing() %>% tk_index() %>% tk_get_timeseries_summary() %>% select(1:4)


# 1 予測力を確認するためのテクニック --------------------------------------------------

# ＜ポイント＞
# - 学習期間のデータを使って予測してみる（予測力向上を促進するチェック方法として役立つ）
# - 今回は｢予測｣がテーマなので、Calibration/Accuracy Testは行わない
#   --- Train/Testでデータ分割せず、それらを合わせたデータを学習期間データとして使用


# * 準備 ----------------------------------------------------

# データ確認
# --- 学習期間データ（以下で使用するデータ）
# --- 全期間のデータを使用している点に注意
data_prepared_tbl %>% tk_index() %>% tk_get_timeseries_summary() %>% select(1:4)


# * 個別モデルの定義 ------------------------------------------

# モデル1：Prophetモデル
# --- 定義＆学習
# --- 学習期間全体のデータで学習している点に注意
model_fit_prophet <-
  prophet_reg(seasonality_yearly = TRUE) %>%
    set_engine("prophet") %>%
    fit(optins_trans ~ optin_time, data = data_prepared_tbl)

# モデル2：ElasticNetモデル
# --- モデル定義
# --- ワークフロー用なので学習なし
model_spec_glmnet <-
  linear_reg(penalty = 0.1, mixture = 0.5) %>%
    set_engine("glmnet")

# モデル2：ElasticNetモデル
# --- ワークフロー設定
# --- 学習期間全体のデータで学習している点に注意
# --- {glmnet}は事前にNA除外の処理が必要（recipe_spec_2_lagにはstep_rm()が入っている）
workflow_fit_glmnet <-
  workflow() %>%
    add_model(model_spec_glmnet) %>%
    add_recipe(recipe_spec_2_lag) %>%
    fit(data = data_prepared_tbl)


# * モデルテーブル作成 -------------------------------------------

# モデルテーブル作成
# --- モデル定義1: Parsnipモデル
# --- モデル定義2: Workflowモデル
model_tbl <-
  modeltime_table(model_fit_prophet,
                  workflow_fit_glmnet)


# * 予測の作成 --------------------------------------------------

# データ確認
# --- 学習期間（訓練データとテストデータの区別なし）
# --- 予測期間（実際に予測する期間）
forecast_tbl %>% tk_index() %>% tk_get_timeseries_summary() %>% select(1:4)
data_prepared_tbl %>% tk_index() %>% tk_get_timeseries_summary() %>% select(1:4)


# ** 学習期間と予測期間を分離 --------------

# 予測データの作成
# --- 各モデルから検証期間の予測を作成
# --- new_data： 予測期間データ
# --- actual_data： 学習期間データ
df_forecast <-
  model_tbl %>%
    modeltime_forecast(new_data    = forecast_tbl,
                       actual_data = data_prepared_tbl)

# プロット作成
# --- 信頼区間なし
df_forecast %>%
  plot_modeltime_forecast(.conf_interval_show = FALSE)


# ** 学習期間のデータを予測 --------------

# 予測データの作成
# --- 各モデルから検証期間の予測を作成
# --- new_data： 学習期間データ（予測期間データではない）
# --- actual_data： 学習期間データ
df_forecast_all <-
  model_tbl %>%
    modeltime_forecast(new_data    = data_prepared_tbl,
                       actual_data = data_prepared_tbl)

# プロット作成
# --- 各モデルから全期間の予測を作成
# --- new_data： 全期間データ
# --- actual_data： 全期間データ
df_forecast_all %>%
  plot_modeltime_forecast(.conf_interval_show = FALSE)



# 2 柔軟にCalibrationを行う ----------------------------------------------------------

# ＜ポイント＞
# - モデルテーブルの学習データを再定義して各モデルを再学習する（Refit）
#   --- 学習期間の全体データから、訓練データに切り替える
# - モデル精度の確認は｢テストデータ｣だけでなく｢訓練データ｣でやることにも意味がある
#   --- modeltime_accuracy()はCalibrationで指定したデータ以外でも指定可能


# * Refitting for Train/Test ----------------------------------------

# Calibration
# --- 訓練データで再学習（学習期間の全体データで学習していた）
# --- テストデータを用いてモデル評価のためのデータを作成
calibration_tbl <-
  model_tbl %>%
    modeltime_refit(training(splits)) %>%
    modeltime_calibrate(testing(splits))


# * Accuracy --------------------------------------------------------

# モデル精度の計測
# --- 検証データ（Out-of-Sample）
# --- Calibrationで設定したデータをそのまま使用
calibration_tbl %>% modeltime_accuracy()

# モデル精度の計測
# --- 訓練データ（In-Sample）
# --- Calibrationと異なるデータを設定することも可能
# --- フィッティング精度は検証データよりも当然高くなる
calibration_tbl %>% 
    modeltime_accuracy(new_data = splits %>% training() %>% drop_na())


# 3 残差分析 -------------------------------------------------------------------------

# ＜ポイント＞
# - 予測精度は残差を見ることで視覚的に確認することができる
# - 残差平均はゼロとなるべきで、個別モデル以外にアンサンブルでも同様のことがいえる
# - 残差分析プロットであるplot_modeltime_residuals()は以下の３パターンの出力が可能
#   --- timeplot
#   --- acf
#   --- seasonality


# * 残差の計算 ------------------------------------------------------------

# 残差の計算
# --- 検証データ（Out-of-Sample）
# --- Nested Dataframeから通常のデータフレームに変換される
# --- ｢.residuals｣の列に追加される
residuals_out_tbl <- calibration_tbl %>% modeltime_residuals()

# 残差の計算
# --- 訓練データ（In-Sample）
# --- 新たなデータをモデルに適用してデータ出力
residuals_in_tbl <-
  calibration_tbl %>%
    modeltime_residuals(new_data = training(splits) %>% drop_na())


# * プロット1：Time Plot -----------------------------------------------------

# プロット作成
# --- 検証データ（Out-of-Sample）
residuals_out_tbl %>% 
    plot_modeltime_residuals(.type = "timeplot",
                             .y_intercept = 0,
                             .y_intercept_color = "blue")

# プロット作成
# --- 訓練データ（In-Sample）
residuals_in_tbl %>%
  plot_modeltime_residuals(.type = "timeplot")


# * プロット2：ACF Plot ------------------------------------------------------

# プロット作成
# --- 検証データ（Out-of-Sample）
residuals_out_tbl %>%
    plot_modeltime_residuals(.type = "acf")


# プロット作成
# --- 訓練データ（In-Sample）
residuals_in_tbl %>%
    plot_modeltime_residuals(.type = "acf")


# * プロット3：Seasonality -----------------------------------------------------

# プロット作成
# --- 検証データ（Out-of-Sample）
residuals_out_tbl %>%
    plot_modeltime_residuals(.type = "seasonality")


# プロット作成
# --- 訓練データ（In-Sample）
residuals_out_tbl %>%
    plot_modeltime_residuals(.type = "seasonality")


# * Forecast ------------------------------------------------------------

# データ確認
# --- 学習期間の全体データ
# --- テストデータ
data_prepared_tbl %>% tk_index() %>% tk_get_timeseries_summary() %>% select(1:4)
splits %>% testing() %>% tk_index() %>% tk_get_timeseries_summary() %>% select(1:4)

# プロット作成
# --- インサンプルデータで予測
calibration_tbl %>%
    modeltime_forecast(new_data = testing(splits),
                       actual_data = data_prepared_tbl) %>%
    plot_modeltime_forecast()

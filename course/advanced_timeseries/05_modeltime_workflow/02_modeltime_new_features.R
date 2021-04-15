# ****************************************************************************
# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: NEW FEATURES - MODELTIME 0.1.0
# ****************************************************************************


# ゴール ----
# MODELTIME 0.1.0のフレキシビリティを確認する
# MODELTIMEを用いた時系列データ分析を確認する

# OBJECTIVES ----
# - Expedited Forecasting - Skip Calibrating / Refitting
# - Working with In-Sample & Out-of-Sample Data
# - NEW Residual Diagnositics


# ＜目次＞
# 0 準備
# 1 EXPEDITED FORECASTING
# 2 CALIBRATION (検証データの作成)
# 3 RESIDUALS（残差分析）


# 0 準備 ------------------------------------------------------------------------------

# LIBRARIES ----------------------------------------------------------------

# Time Series ML
library(tidymodels)
library(modeltime)

# Core
library(tidyverse)
library(timetk)
library(lubridate)



# DATA & ARTIFACTS --------------------------------------------------------

# データ準備
feature_engineering_artifacts_list <-
  read_rds("00_models/feature_engineering_artifacts_list.rds")


# データ取得
data_prepared_tbl    <- feature_engineering_artifacts_list$data$data_prepared_tbl
forecast_tbl         <- feature_engineering_artifacts_list$data$forecast_tbl

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


# 1 EXPEDITED FORECASTING -----------------------------------------------------------------

# * Model Time Table ----
#   - Fitted on Full Dataset (No Train/Test)

# モデル定義1
# --- Prophetモデル
model_fit_prophet <-
  prophet_reg(seasonality_yearly = TRUE) %>%
    set_engine("prophet") %>%
    fit(optins_trans ~ optin_time, data = data_prepared_tbl)


# モデル定義2
# --- ElasticNetモデル
model_spec_glmnet <-
  linear_reg(penalty = 0.1, mixture = 0.5) %>%
    set_engine("glmnet")


# ワークフロー設定
# --- モデル定義2を使用
# --- {glmnet}は事前にエラー処理が必要
# --- recipe_spec_2_lagにはエラー処理が入っている
workflow_fit_glmnet <-
  workflow() %>%
    add_model(model_spec_glmnet) %>%
    add_recipe(recipe_spec_2_lag) %>%
    fit(data_prepared_tbl)


# モデルテーブル作成
# --- モデル定義1: Parsnipモデル
# --- モデル定義2: Workflowモデル
model_tbl <-
  modeltime_table(model_fit_prophet,
                  workflow_fit_glmnet)



# * Make a Forecast ------------------------------------------------------------------------

#   - 信頼区間なし----

# プロット作成
# --- 各モデルから検証期間の予測を作成
# --- new_data： 検証データ
# --- actual_data： 全期間データ
model_tbl %>%
    modeltime_forecast(new_data    = forecast_tbl,
                       actual_data = data_prepared_tbl) %>%
    plot_modeltime_forecast()

# プロット作成
# --- 各モデルから全期間の予測を作成
# --- new_data： 全期間データ
# --- actual_data： 全期間データ
model_tbl %>%
    modeltime_forecast(new_data    = data_prepared_tbl,
                       actual_data = data_prepared_tbl) %>%
    plot_modeltime_forecast()



# 2 CALIBRATION (検証データの作成) ---------------------------------------------------

# * Refitting for Train/Test ------------------------------------------------

# 予測データの作成
# --- リフィットの実行（訓練データに基づいて再度学習）
# --- テストデータからモデル精度を評価するためのデータを作成
calibration_tbl <-
  model_tbl %>%
    modeltime_refit(training(splits)) %>%
    modeltime_calibrate(testing(splits))


# * Accuracy ----------------------------------------------------------------

# モデル精度の計測
# --- 検証データ（Out-of-Sample）
calibration_tbl %>% modeltime_accuracy()

# モデル精度の計測
# --- 訓練データ（In-Sample）
# --- フィッティング精度は検証データよりも当然高い
calibration_tbl %>% 
    modeltime_accuracy(new_data = splits %>% training() %>% drop_na())


# 3 RESIDUALS（残差分析） -------------------------------------------------------------------------

# ＜ポイント＞
# - 予測精度は残差を見ることで視覚的に確認することができる
# - plot_modeltime_residuals()は以下の３パターンのプロット作成が可能
#   --- timeplot
#   --- acf
#   --- seasonality


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


# * Time Plot ------------------------------------------------------------

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


# * ACF Plot ---------------------------------------------------------------

# プロット作成
# --- 検証データ（Out-of-Sample）
residuals_out_tbl %>%
    plot_modeltime_residuals(.type = "acf")


# プロット作成
# --- 訓練データ（In-Sample）
residuals_in_tbl %>%
    plot_modeltime_residuals(.type = "acf")


# * Seasonality ------------------------------------------------------------

# プロット作成
# --- 検証データ（Out-of-Sample）
residuals_out_tbl %>%
    plot_modeltime_residuals(.type = "seasonality")


# プロット作成
# --- 訓練データ（In-Sample）
residuals_out_tbl %>%
    plot_modeltime_residuals(.type = "seasonality")



# * Forecast ------------------------------------------------------------

# プロット作成
# --- In-Sample
# --- 予測データの作成
calibration_tbl %>%
    modeltime_forecast(
        new_data = testing(splits),
        actual_data = data_prepared_tbl
    ) %>%
    plot_modeltime_forecast()




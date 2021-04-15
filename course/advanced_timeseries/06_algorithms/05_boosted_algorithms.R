# *****************************************************************************
# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: BOOSTED ALGORITHMS
# *****************************************************************************


# GOAL:
# Understand Boosting Errors

# OBJECTIVES ----
# - Learn about modeling residual errors
# - Apply Prophet + XGBoost


# ＜目次＞
# 0 準備
# 1 PROPHET BOOST
# 2 ARIMA BOOST
# 3 MODELTIMEによる評価


# 0 準備 ------------------------------------------------------------------------

# LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)

# Core
library(tidyverse)
library(lubridate)
library(timetk)

# 関数
source("00_scripts/01_calibrate_and_plot.R")


# * データ準備 ---------------------------------------------------------

# データロード
artifacts_list <- read_rds("00_models/feature_engineering_artifacts_list.rds") 

# データ取得
data_prepared_tbl <- artifacts_list$data$data_prepared_tbl 

# データ確認
data_prepared_tbl %>% print()
data_prepared_tbl %>% glimpse()
data_prepared_tbl %>% tk_index() %>% tk_get_timeseries_summary()


# * モデル準備 ---------------------------------------------------------

# モデルロード
model_fit_best_prophet <- read_rds("00_models/model_fit_best_prophet.rds")
model_fit_best_arima   <- read_rds("00_models/model_fit_best_arima.rds")


# * データ分割 ----------------------------------------------------------

# 時系列データ分割
splits <-
  data_prepared_tbl %>%
    time_series_split(assess = "8 weeks", cumulative = TRUE)

# プロット作成
# --- 訓練期間とテスト期間の確認
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)


# 1 PROPHET BOOST ------------------------------------------------------------------

# * Best Prophet Model --------------------------------------------

# モデル確認
model_fit_best_prophet %>% print()

# フォーミュラ抽出
model_fit_best_prophet$preproc$terms %>% formula()


# Error - Broken Model

calibrate_and_plot(model_fit_best_prophet, type = "testing")


# Fixing a broken model

model_tbl_best_prophet <-
  modeltime_table(model_fit_best_prophet) %>%
    modeltime_refit(training(splits))

model_tbl_best_prophet$.model[[1]]

model_fit_best_prophet_refitted <- model_tbl_best_prophet %>% pluck(".model", 1)

calibrate_and_plot(model_fit_best_prophet_refitted)


# * Boosting Prophet Models -----------------------------------

# Recipes

# レシピ作成
recipe_spec_base_no_lag <-
  artifacts_list$recipes$recipe_spec_base %>%
    step_rm(starts_with("lag"))

# データ確認
recipe_spec_base_no_lag %>% prep() %>% juice() %>% glimpse()

# Model Spec
# モデル構築
model_spec_prophet_boost <-
  prophet_boost(# Prophet Params
                changepoint_num    = 25,
                changepoint_range  = 0.8,
                seasonality_daily  = FALSE,
                seasonality_weekly = FALSE,
                seasonality_yearly = FALSE,
                # Xgboost
                mtry           = 0.75,
                min_n          = 20,
                tree_depth     = 3,
                learn_rate     = 0.2,
                loss_reduction = 0.15,
                trees          = 300) %>%
    set_engine("prophet_xgboost")


# ワークフロー設定
set.seed(123)
wflw_fit_prophet_boost <-
  workflow() %>%
    add_model(model_spec_prophet_boost) %>%
    add_recipe(recipe_spec_base_no_lag) %>%
    fit(training(splits))

# 確認
wflw_fit_prophet_boost %>% print()

# プロット作成
# --- 予測データの確認
calibrate_and_plot(model_fit_best_prophet_refitted,
                   wflw_fit_prophet_boost, type = "testing")

# プロット作成
# --- 残差分析
modeltime_table(wflw_fit_prophet_boost) %>%
  modeltime_residuals(testing(splits)) %>%
  plot_modeltime_residuals()


# 2 ARIMA BOOST ----------------------------------------------------------------

# * Best ARIMA Model --------------------------------------------

# モデル確認
model_fit_best_arima %>% print()

# プロット作成
calibrate_and_plot(model_fit_best_arima)

# フォーミュラ取得
model_fit_best_arima$preproc$terms %>% formula()


# * Boosting ARIMA ---------------------------------------------

# モデル構築
model_spec_arima_boost <-
  arima_boost(# ARIMA
              seasonal_period = 1,
              # Xgboost
              mtry           = 0.75,
              min_n          = 20,
              tree_depth     = 3,
              learn_rate     = 0.25,
              loss_reduction = 0.15,
              trees          = 300) %>%
    set_engine("auto_arima_xgboost")

# ワークフロー設定
set.seed(123)
wflw_fit_arima_boost <-
  wflw_fit_prophet_boost %>%
    update_model(model_spec_arima_boost) %>%
    fit(training(splits))

# 確認
wflw_fit_arima_boost %>% print()

# プロット作成
# --- 予測データの確認
calibrate_and_plot(wflw_fit_arima_boost, type = "testing")


# 3 MODELTIMEによる評価 --------------------------------------------------------

# モデルテーブル登録
model_tbl <-
  modeltime_table(# Prophet
                  model_fit_best_prophet_refitted,
                  wflw_fit_prophet_boost,
                  # ARIMA
                  model_fit_best_arima,
                  wflw_fit_arima_boost)

# モデル評価/予測の準備
calibration_tbl <-
  model_tbl %>%
    modeltime_calibrate(testing(splits))

# モデル精度の評価
calibration_tbl %>% modeltime_accuracy()

# プロット作成
# --- 予測データの確認
calibration_tbl %>%
    modeltime_forecast(
        new_data = testing(splits),
        actual_data = data_prepared_tbl
    ) %>%
    plot_modeltime_forecast()


# リフィット
refit_tbl <-
  calibration_tbl %>%
    modeltime_refit(data = data_prepared_tbl)

refit_tbl %>%
    modeltime_forecast(new_data = artifacts_list$data$forecast_tbl,
                       actual_data = data_prepared_tbl) %>%
    plot_modeltime_forecast(.conf_interval_show = FALSE)


# 4 結果保存 ----------------------------------------------------------------

# モデル保存
#calibration_tbl %>%
#    write_rds("00_models/calibration_tbl_boosted_models.rds")


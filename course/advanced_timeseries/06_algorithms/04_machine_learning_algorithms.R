# ****************************************************************************
# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: MACHINE LEARNING
# ****************************************************************************


# ゴール ----
# - 機械学習アルゴリズムと時系列への適用方法を理解する

# OBJECTIVES ----
# - 主要な機械学習アルゴリズムを確認する
# - アルゴリズムのハイパーパラメータを確認する
# - Modeltimeのワークフローで分析する


# ＜目次＞
# 0 準備
# 1 ElasticNet回帰
# 2 MARS
# 3 SVM POLY
# 4 SVM RADIAL BASIS
# 5 k近傍法
# 6 ランダムフォレスト
# 7 XGBOOST
# 8 アソシエーション分析
# 9 ニューラルネット
# 10 ニューラルネットAR
# 11 Modeltimeによる予測フロー
# 12 モデル保存


# 0 準備 ------------------------------------------------------------------------

# LIBRARIES & SETUP ----------------------------------------------------------------

# Time Series ML
library(tidymodels)
library(modeltime)
library(rules)

# Core
library(tidyverse)
library(lubridate)
library(timetk)

# Function
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


# * データ分割 ---------------------------------------------

# 時系列データ分割
splits <-
  data_prepared_tbl %>%
    time_series_split(assess = "8 weeks", cumulative = TRUE)

# データ確認
splits %>% training() %>% glimpse()

# プロット作成
# --- 訓練期間とテスト期間の確認
splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)


# * レシピ準備 ---------------------------------------------

# 基本レシピ
recipe_spec_base <- artifacts_list$recipes$recipe_spec_base
recipe_spec_base %>% tidy()

# スプラインレシピ
recipe_spec_1_spline <- artifacts_list$recipes$recipe_spec_1
recipe_spec_1_spline %>% tidy()

# ラグレシピ
recipe_spec_2_lag <- artifacts_list$recipes$recipe_spec_2
recipe_spec_2_lag %>% tidy()

# レシピ適用
recipe_spec_base %>% prep() %>% juice() %>% glimpse()
recipe_spec_1_spline %>% prep() %>% juice() %>% glimpse()
recipe_spec_2_lag %>% prep() %>% juice() %>% glimpse()


# 1 ElasticNet回帰 ------------------------------------------------

# ＜ポイント＞
# - 長所
#   - トレンドを的確に学習することができる
# - 短所:
#   - 複雑なパターンを学習することはできない (i.e. seasonality)

# モデル構築
model_spec_glmnet <-
  linear_reg(mode = "regression",
             penalty = 0.01,
             mixture = 0) %>%
    set_engine("glmnet")

# ワークフロー設定
# --- レシピ： recipe_spec_1_spline
wflw_fit_glmnet_spline <-
  workflow() %>%
    add_model(model_spec_glmnet) %>%
    add_recipe() %>%
    fit(training(splits))

# ワークフロー設定
# --- レシピ： recipe_spec_2_lag
wflw_fit_glmnet_lag <-
  workflow() %>%
    add_model(model_spec_glmnet) %>%
    add_recipe(recipe_spec_2_lag) %>%
    fit(training(splits))

# モデル学習
# --- モデルテーブルに登録
# --- Descriptionの更新
# --- モデル検証と予測の準備
calibration_tbl <-
  modeltime_table(wflw_fit_glmnet_spline,
                  wflw_fit_glmnet_lag) %>%
    update_model_description(1, "GLMNET - Spline") %>%
    update_model_description(2, "GLMNET - Lag") %>%
    modeltime_calibrate(testing(splits))

# モデル精度の検証
calibration_tbl %>% modeltime_accuracy()

# プロット作成
# --- 予測データの確認
calibration_tbl %>%
  modeltime_forecast(new_data = testing(splits),
                     actual_data = data_prepared_tbl) %>%
  plot_modeltime_forecast(.conf_interval_show = FALSE)

# 一括処理
# --- 検証/予測準備からプロット作成
calibrate_and_plot(wflw_fit_glmnet_spline,
                   wflw_fit_glmnet_lag, type = "testing")


# 2 MARS --------------------------------------------------------------------------------

# ＜ポイント＞
# Multiple Adaptive Regression Splines
# - 長所:
#   - トレンドを学習するためのベストなモデル
# - 短所:
#   - 複雑なパターンを学習することはできない (i.e. seasonality)
#   - MARS自身がスプラインを使うため、前処理でスプラインを使うことができない.

# ＜応用＞
# - XGBoostと融合して使うことができる (季節性の追跡力が改善)
#   - prophet_reg: uses a technique similar to mars for modeling trend component
#   - prophet_boost: Uses prophet for trend, xgboost for features


# モデル構築
model_spec_mars <-
  mars(mode = "regression",
       num_terms = 10) %>%
    set_engine("earth", endspan = 200)

# ワークフロー設定
# --- レシピ： 使用なし（スプラインが重複するため）
# --- フォーミュラ設定
wflw_fit_mars_simple <-
  workflow() %>%
    add_model(model_spec_mars) %>%
    add_formula(optins_trans ~ as.numeric(optin_time)) %>%
    fit(training(splits))

# プロット作成
calibrate_and_plot(wflw_fit_mars_simple, type = "testing")

# ワークフロー設定
# --- レシピ： recipe_spec_1_spline
wflw_fit_mars_spline <-
  workflow() %>%
    add_model(model_spec_mars) %>%
    add_recipe(recipe_spec_1_spline) %>%
    fit(training(splits))

# ワークフロー設定
# --- レシピ： recipe_spec_2_lag
wflw_fit_mars_lag <-
  workflow() %>%
    add_model(model_spec_mars) %>%
    add_recipe(recipe_spec_2_lag) %>%
    fit(training(splits))

# Calibrate & Plot
calibrate_and_plot(wflw_fit_mars_simple,
                   wflw_fit_mars_spline,
                   wflw_fit_mars_lag)


# 3 SVM POLY ------------------------------------------------------------------------

# 長所:
#  - バランスのとれたアルゴリズム
# 短所:
#  - チューニングが必要
#  - オーバーフィットしやすい

# モデル構築
model_spec_svm_poly <-
  svm_poly(mode = "regression",
           degree = 1,
           scale_factor = 1,
           cost = 10,
           margin = 0.1) %>%
    set_engine("kernlab")

# ワークフロー設定
# --- レシピ： recipe_spec_1_spline
set.seed(123)
wflw_fit_svm_poly_spline <-
  workflow() %>%
    add_model(model_spec_svm_poly) %>%
    add_recipe(recipe_spec_1_spline) %>%
    fit(training(splits))

# プロット作成
calibrate_and_plot(wflw_fit_svm_poly_spline, type = "testing")

# ワークフロー設定
# --- レシピ： recipe_spec_2_lag
set.seed(123)
wflw_fit_svm_poly_lag <-
  workflow() %>%
    add_model(model_spec_svm_poly) %>%
    add_recipe(recipe_spec_2_lag) %>%
    fit(training(splits))

# プロット作成
calibrate_and_plot(wflw_fit_svm_poly_spline,
                   wflw_fit_svm_poly_lag, type = "testing")


# 4 SVM RADIAL BASIS ------------------------------------------------------------------

# 長所:
#  - バランスのとれたアルゴリズム
# 短所:
#  - チューニングが必要
#  - オーバーフィットしやすい


# モデル構築
model_spec_svm_rbf <-
  svm_rbf(mode = "regression",
          cost = 1,
          rbf_sigma = 0.01,
          margin = 0.1) %>%
    set_engine("kernlab")

# ワークフロー設定
# --- レシピ： recipe_spec_1_spline
wflw_fit_svm_rbf_spline <-
  workflow() %>%
    add_model(model_spec_svm_rbf) %>%
    add_recipe(recipe_spec_1_spline) %>%
    fit(training(splits))

# プロット作成
calibrate_and_plot(wflw_fit_svm_rbf_spline, type = "testing")

# ワークフロー設定
# --- レシピ： recipe_spec_2_lag
wflw_fit_svm_rbf_lag <-
  wflw_fit_svm_rbf_spline %>%
    update_recipe(recipe_spec_2_lag) %>%
    fit(training(splits))

# プロット作成
calibrate_and_plot(wflw_fit_svm_rbf_spline,
                   wflw_fit_svm_rbf_lag, type = "testing")


# 5 k近傍法 ----------------------------------------------------------------------------

# - 長所:
#   - 推定に近傍点を使うことができる
# - 短所:
#   - 訓練期間の最大/最小を超えた予測ができない (e.g. 上昇トレンド/下落トレンド)
# - Solution: Model trend separately (if needed). 
#   - Can combine with ARIMA, Linear Regression, Mars, or Prophet


# 課題確認： KNNは上昇トレンドを追随できない ------------------------

# 時系列データ作成
# --- 単調増加のデータ
sample_data_tbl <-
  tibble(date = tk_make_timeseries("2011", by = "quarter", length_out = 10),
         value = 1:10)

# プロット作成
sample_data_tbl %>% plot_time_series(date, value, .smooth = FALSE)

# 将来データの結合
simple_new_data <-
  bind_rows(sample_data_tbl,
            future_frame(sample_data_tbl, .length_out = "2 years"))

# モデル構築
# --- k近傍法
simple_fit_knn <-
  nearest_neighbor(mode = "regression") %>%
    set_engine("kknn") %>%
    fit(value ~ as.numeric(date), sample_data_tbl)

# モデル構築
# --- k近傍法
simple_fit_glmnet <-
  linear_reg(penalty = 0.000, mixture = 0.5) %>%
    set_engine("glmnet") %>%
    fit(value ~ as.numeric(date) + quarter(date), sample_data_tbl)

# プロット作成
# --- k近傍法が上昇トレンドを予測できないことを確認
modeltime_table(simple_fit_knn,
                simple_fit_glmnet) %>%
  modeltime_forecast(new_data = simple_new_data,
                     actual_data = sample_data_tbl) %>%
  plot_modeltime_forecast()


# KNNの実行 ------------------------------------------------------------

# モデル構築
model_spec_knn <-
  nearest_neighbor(mode = "regression",
                   neighbors = 50,
                   dist_power = 10,
                   weight_func = "optimal") %>%
    set_engine("kknn")

# ワークフロー設定
# --- レシピ： recipe_spec_1_spline
set.seed(123)
wflw_fit_knn_spline <- workflow() %>%
  add_model(model_spec_knn) %>%
  add_recipe(recipe_spec_1_spline) %>%
  fit(training(splits))

# プロット作成
calibrate_and_plot(wflw_fit_knn_spline, type = "testing")

# ワークフロー設定
# --- レシピ： recipe_spec_2_lag
set.seed(123)
wflw_fit_knn_lag <- wflw_fit_knn_spline %>%
  update_recipe(recipe_spec_2_lag) %>%
  fit(training(splits))


# Calibrate & Plot

calibrate_and_plot(wflw_fit_knn_spline,
                   wflw_fit_knn_lag, type = "testing")

# 6 ランダムフォレスト --------------------------------------------------------------------

# - 長所:
#   - 季節性をうまく表現することができる
# - 短所:
#   - 訓練期間の最大/最小を超えた予測ができない (e.g. 上昇トレンド/下落トレンド)
# - Solution: Model trend separately (if needed). 
#   - Can combine with ARIMA, Linear Regression, Mars, or Prophet

# モデル構築
model_spec_rf <-
  rand_forest(mode = "regression",
              mtry = 25,
              trees = 1000,
              min_n = 25) %>%
    set_engine("randomForest")

# ワークフロー設定
# --- レシピ： recipe_spec_1_spline
set.seed(123)
wflw_fit_rf_spline <-
  workflow() %>%
    add_model(model_spec_rf) %>%
    add_recipe(recipe_spec_1_spline) %>%
    fit(training(splits))

# プロット作成
calibrate_and_plot(wflw_fit_rf_spline, type = "testing")

# ワークフロー設定
# --- レシピ： recipe_spec_2_lag
wflw_fit_rf_lag <-
  wflw_fit_rf_spline %>%
    update_recipe(recipe_spec_2_lag) %>%
    fit(training(splits))

# プロット作成
calibrate_and_plot(wflw_fit_rf_spline,
                   wflw_fit_rf_lag, type = "testing")


# 7 XGBOOST ------------------------------------------------------------------------

# - 長所:
#   - 季節性や複雑なパターンを表現することができる
# - 短所:
#   - 訓練期間の最大/最小を超えた予測ができない (e.g. 上昇トレンド/下落トレンド)
# - Solution: Model trend separately (if needed). 
#   - Can combine with ARIMA, Linear Regression, Mars, or Prophet
#   - prophet_boost & arima_boost: Do this


# モデル構築
model_spec_boost <-
  boost_tree(mode = "regression",
             mtry = 25,
             trees = 1000,
             min_n = 2,
             tree_depth = 12,
             learn_rate = 0.3,
             loss_reduction = 0) %>%
    set_engine("xgboost")

# ワークフロー設定
# --- レシピ： recipe_spec_1_spline
set.seed(123)
wflw_fit_xgboost_spline <-
  workflow() %>%
    add_model(model_spec_boost) %>%
    add_recipe(recipe_spec_1_spline) %>%
    fit(training(splits))

# ワークフロー設定
# --- レシピ： recipe_spec_2_lag
set.seed(123)
wflw_fit_xgboost_lag <- wflw_fit_xgboost_spline %>%
    update_recipe(recipe_spec_2_lag) %>%
    fit(training(splits))

# プロット作成
calibrate_and_plot(wflw_fit_xgboost_spline,
                   wflw_fit_xgboost_lag, type = "testing")


# 8 アソシエーション分析 --------------------------------------------------------------------

# - Like XGBoost, but the terminal (final) nodes are fit using linear regression
# - Does better than tree-based algorithms when time series has trend
# - Can predict beyond maximum

# ＜エラー＞
# Error in rlang::env_get(mod_env, items) (04_machine_learning_algorithms.R#486): argument "default" is missing, with no default
#Show stack trace


# モデル構築
model_spec_cubist <-
  cubist_rules(mode = "regression",
               committees = 50,
               neighbors = 7,
               max_rules = 100) %>%
    set_engine("Cubist")

# ワークフロー設定
# --- レシピ： recipe_spec_1_spline
set.seed(123)
wflw_fit_cubist_spline <-
  workflow() %>%
    add_model(model_spec_cubist) %>%
    add_recipe(recipe_spec_1_spline) %>%
    fit(training(splits))

# ワークフロー設定
# --- レシピ： recipe_spec_2_lag
set.seed(123)
wflw_fit_cubist_lag <-
  wflw_fit_cubist_spline %>%
    update_recipe(recipe_spec_2_lag) %>%
    fit(training(splits))

# プロット作成
calibrate_and_plot(wflw_fit_cubist_spline,
                   wflw_fit_cubist_lag, type = "testing")


# 9 ニューラルネット ------------------------------------------------------------------------

# - Single Layer Multi-layer Perceptron Network
# - Simple network - Like linear regression
# - Can improve learning by adding more hidden units, epochs, etc


# モデル構築
model_spec_nnet <-
  mlp(mode = "regression",
      hidden_units = 10,
      penalty = 1,
      epochs = 100) %>%
    set_engine("nnet")

# ワークフロー設定
# --- レシピ： recipe_spec_1_spline
set.seed(123)
wflw_fit_nnet_spline <-
  workflow() %>%
    add_model(model_spec_nnet) %>%
    add_recipe(recipe_spec_1_spline) %>%
    fit(training(splits))

# ワークフロー設定
# --- レシピ： recipe_spec_2_lag
set.seed(123)
wflw_fit_nnet_lag <-
  wflw_fit_nnet_spline %>%
    update_recipe(recipe_spec_2_lag) %>%
    fit(training(splits))

# プロット作成
calibrate_and_plot(wflw_fit_nnet_spline,
                    wflw_fit_nnet_lag, type = "testing")


# 10 ニューラルネットAR ------------------------------------------------------------------------

# - NNET with Lagged Features (AR)
# - Is a sequential model (comes from the forecast package)
# - Must include date feature
# Base Model


# ヘルプ参照
?nnetar_reg

# 学習データの確認
recipe_spec_base %>% prep() %>% juice() %>% glimpse()

# モデル構築
model_spec_nnetar <-
  nnetar_reg(non_seasonal_ar = 2,
             seasonal_ar     = 1,
             hidden_units    = 10,
             penalty         = 10,
             num_networks    = 10,
             epochs          = 50) %>%
    set_engine("nnetar")

# ワークフロー設定
# --- レシピ： recipe_spec_base
set.seed(123)
wflw_fit_nnetar_base <-
  workflow() %>%
    add_model(model_spec_nnetar) %>%
    add_recipe(recipe_spec_base) %>%
    fit(training(splits) %>% drop_na())

# プロット作成
calibrate_and_plot(wflw_fit_nnetar_base, type = "testing")


# 11 Modeltimeによる予測フロー ------------------------------------------------------------

# - Compare model performance

# * Modeltime Table ----

# モデルテーブルに登録
model_tbl <-
  modeltime_table(wflw_fit_glmnet_spline,
                  wflw_fit_glmnet_lag,
                  wflw_fit_mars_spline,
                  wflw_fit_mars_lag,
                  wflw_fit_svm_poly_spline,
                  wflw_fit_svm_poly_lag,
                  wflw_fit_svm_rbf_spline,
                  wflw_fit_svm_rbf_lag,
                  wflw_fit_knn_spline,
                  wflw_fit_knn_lag,
                  wflw_fit_rf_spline,
                  wflw_fit_rf_lag,
                  wflw_fit_xgboost_spline,
                  wflw_fit_xgboost_lag,
                  wflw_fit_cubist_spline,
                  wflw_fit_cubist_lag,
                  wflw_fit_nnet_spline,
                  wflw_fit_nnet_lag,
                  wflw_fit_nnetar_base) %>%
    mutate(.model_desc_2 = str_c(.model_desc,
                                 rep_along(.model_desc, c(" - Spline", " - Lag"))),
           .model_desc = ifelse(.model_id == 19, .model_desc, .model_desc_2)) %>%
    select(-.model_desc_2)


# * Calibration Table ----

calibration_tbl <-
  model_tbl %>%
    modeltime_calibrate(testing(splits))

calibration_tbl


# * Obtain Test Forecast Accuracy ----

calibration_tbl %>%
    modeltime_accuracy() %>%
    table_modeltime_accuracy(resizable = TRUE, bordered = TRUE)


# * Visualize Test Forecast ----

forecast_test_tbl <-
  calibration_tbl %>%
    modeltime_forecast(new_data    = testing(splits),
                       actual_data = data_prepared_tbl)

forecast_test_tbl %>%
    plot_modeltime_forecast(.conf_interval_show = FALSE)


# * Refit ----

set.seed(123)
refit_tbl <-
  calibration_tbl %>%
    modeltime_refit(data = data_prepared_tbl)

forecast_future_tbl <-
  refit_tbl %>%
    modeltime_forecast(new_data    = artifacts_list$data$forecast_tbl,
                       actual_data = data_prepared_tbl)

forecast_future_tbl %>%
    plot_modeltime_forecast(.conf_interval_show = FALSE)


# 12 モデル保存 --------------------------------------------------------------------

# モデル保存
#calibration_tbl %>%
#    write_rds("00_models/machine_learning_calibration_tbl.rds")

# モデルのインポート
#read_rds("00_models/machine_learning_calibration_tbl.rds")


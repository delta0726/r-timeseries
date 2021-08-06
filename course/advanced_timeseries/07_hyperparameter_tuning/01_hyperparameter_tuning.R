# *****************************************************************************
# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: HYPERPARAMETER TUNING 
# *****************************************************************************


# SPECIAL REQUIREMENTS:
# - Parallel Processing: Requires development versions of:
#   tune, recipes, workflows, modeltime, and timetk

# GOAL:
# モデルのチューニング方法を知る

# OBJECTIVES ----
# - Sequential (e.g. ARIMA, ETS, TBATS) vs Non-Sequential Algorithms (e.g. Prophet, ML)
# - Cross-Validation Workflow（K-FOLD vs TSCV）
# - Hyperparameter tuning with Prophet Boost
# - Parallel Processing with doFuture for 3X-5X Speedup


# ＜目次＞
# 0 準備
# 1 REVIEW
# 2 ニューラルネットAR
# 3 PROPHET BOOST


# 0 準備 -----------------------------------------------------------------------------

# * Change Locale -----------------------------------------------------

Sys.setlocale("LC_TIME", "English")
Sys.getlocale("LC_TIME")


# * LIBRARIES & SETUP --------------------------------------------------

# Time Series ML
library(tidymodels)
library(modeltime)
library(rules)

# Timing & Parallel Processing
library(tictoc)
library(future)
library(doFuture)

# Core 
library(plotly)
library(tidyverse)
library(lubridate)
library(timetk)

# Function
source("00_scripts/01_calibrate_and_plot.R")


# * DATA ----------------------------------------------------------------

# データロード
artifacts_list <- read_rds("00_models/feature_engineering_artifacts_list.rds")

# データ取得
data_prepared_tbl <- artifacts_list$data$data_prepared_tbl

# データ確認
data_prepared_tbl %>% print()


# * MODELS ----------------------------------------------------------------

# モデルロード
# --- モデルテーブル形式
calibration_ml_tbl      <- read_rds("00_models/machine_learning_calibration_tbl.rds")
calibration_boosted_tbl <- read_rds("00_models/calibration_tbl_boosted_models.rds")
calibration_ets_tbl     <- read_rds("00_models/calibration_tbl_ets_tbats.rds")


# * データ分割 ------------------------------------------------------------

# 時系列データ分割
splits <-
  data_prepared_tbl %>%
    time_series_split(assess = "8 weeks", cumulative = TRUE)

# プロット作成
# --- 訓練データ/テストデータ
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)


# 1 REVIEW ------------------------------------------------------------------------

# * テーブル結合 ------------------------------------------------------

# モデルテーブルの確認
calibration_ml_tbl %>% print()
calibration_boosted_tbl %>% print()
calibration_ets_tbl %>% print()

# テーブル結合
model_tbl <-
  combine_modeltime_tables(calibration_ml_tbl,
                           calibration_boosted_tbl,
                           calibration_ets_tbl)


# * Review Accuracy ----

# テストデータの適用
calibration_tbl <-
  model_tbl %>%
    modeltime_calibrate(testing(splits))

# モデル精度の検証
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(defaultPageSize = 30, bordered = TRUE, resizable = TRUE)



# 2 ニューラルネットAR ----------------------------------------------------------------


# [SEQUENTIAL MODEL] ------------------------------------------------

# ＜シーケンシャルモデル＞
# - [定義]：
#   - ラグを内部的に生成する
#   - [IMPORTANT DIFFERENTIATION]: Predicts next H observations
#   - All data must be sequential
#   - K-Foldクロスバリデーションが使えない（時系列クロスバリデーションを使う）
# - [例]：
#   - ARIMA
#   - Exponential Smoothing
#   - NNETAR
#   - Any algorithm from the forecast package


# * モデル抽出 --------------------------------------------------------

# モデル抽出
wflw_fit_nnetar <-
  calibration_tbl %>%
    pluck_modeltime_model(19)

# 確認
wflw_fit_nnetar %>% print()


# * クロスバリデーション計画の設定 ----------------------------------------

# ヘルプ参照
?time_series_cv

# 時系列クロスバリデーションの設定
resamples_tscv_lag <-
  splits %>%
    training() %>%
    drop_na() %>%
    time_series_cv( cumulative  = TRUE,
                    initial     = "6 months",
                    assess      = "8 weeks",
                    skip        = "4 weeks",
                    slice_limit = 6)

# プロット作成
# --- 訓練データ / テストデータ
resamples_tscv_lag %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)


# * レシピ設定 ----------------------------------------------------------

# レシピ追加
recipe_spec_3_lag_date <-
  wflw_fit_nnetar %>%
    pull_workflow_preprocessor() %>%
    step_naomit(starts_with("lag"))

# データ確認
recipe_spec_3_lag_date %>% prep() %>% juice() %>% glimpse()


# * モデル設定 ------------------------------------------------------------

# モデルパラメータの確認
# --- ニューラルネットARモデル
wflw_fit_nnetar %>% pull_workflow_spec()

# チューニング設定
model_spec_nnetar <-
  nnetar_reg(seasonal_period = 7,
             non_seasonal_ar = tune(id = "non_seasonal_ar"),
             seasonal_ar     = tune(),
             hidden_units    = tune(),
             num_networks    = 10,
             penalty         = tune(),
             epochs          = 50) %>%
    set_engine("nnetar")

# パラメータ確認
model_spec_nnetar %>% parameters()

# デフォルトのチューニング範囲
modeltime::non_seasonal_ar()
seasonal_ar()
hidden_units()
penalty() %>% dials::value_sample(5)


# * チューニンググリッドの設定 ------------------------------------------

# ランダムサーチ
# --- 乱数を用いるのでシード設定
set.seed(123)
model_spec_nnetar %>%
  parameters() %>%
  grid_random(size = 10)


# ヘルプ参照
# --- ラテン式ハイパーキューブ
?grid_latin_hypercube()

# ラテンハイパーキューブサーチ
set.seed(123)
grid_spec_nnetar_1 <-
  model_spec_nnetar %>%
    parameters() %>%
    grid_latin_hypercube(size = 15)


# ラテンハイパーキューブサーチ
# --- チューニング範囲指定
# --- {dials}又は{modeltime}からチューニング範囲の関数より取得
set.seed(123)
grid_spec_nnetar_2 <-
  grid_latin_hypercube(
    non_seasonal_ar(range = c(1, 3)),
    seasonal_ar(range = c(2, 2)),
    hidden_units(range = c(2, 5)),
    penalty(range = c(-4.8, -2.9), trans = scales::log10_trans()),
    size = 15
)


# * チューニング ------------------------------------------------------

# ＜ポイント＞
# - 計算コストのかかる処理（並列処理が必須）


# ヘルプ参照
?tune_grid

# ワークフロー更新
# --- 処理を追加したレシピ
# --- チューニング用のモデル
wflw_tune_nnetar <-
  wflw_fit_nnetar %>%
    update_recipe(recipe_spec_3_lag_date) %>%
    update_model(model_spec_nnetar)
    

# ** 並列化の設定 -------------------------------------

# 並列化の開始
registerDoFuture()

# コア数の取得
n_cores <- parallel::detectCores()

# ヘルプ参照
# ?plan
plan(strategy = cluster,
     workers  = parallel::makeCluster(n_cores))


# ** 時系列クロスバリデーション ------------------------

# Round 1
# --- 33.8 sec elapsed
tic()
set.seed(123)
tune_results_nnetar_1 <-
  wflw_tune_nnetar %>%
    tune_grid(resamples = resamples_tscv_lag,
              grid      = grid_spec_nnetar_1,
              metrics   = default_forecast_accuracy_metric_set(),
              control   = control_grid(verbose = TRUE, save_pred = TRUE))
toc()

# Round 2
# --- 20.84 sec elapsed
tic()
set.seed(123)
tune_results_nnetar_2 <-
  wflw_tune_nnetar %>%
    tune_grid(resamples = resamples_tscv_lag,
              grid      = grid_spec_nnetar_2,
              metrics   = default_forecast_accuracy_metric_set(),
              control   = control_grid(verbose = TRUE, save_pred = TRUE))
toc()


# ** 並列化終了 --------------------------------------

plan(strategy = sequential)


# 結果確認 -----------------------------------------------------

# データ確認
# --- Tibble形式
tune_results_nnetar_2

# 上位ランキング
# --- meanが小さい順に出力
tune_results_nnetar_2 %>% show_best(metric = "rmse", n = Inf)

# プロット作成
# --- 横軸：パラメータ
# --- 縦軸：メトリック
g <-
  tune_results_nnetar_2 %>%
    autoplot() +
    geom_smooth(se = FALSE)

# プロット更新
# --- インタラクティブ変換
ggplotly(g)


# * Retrain & Assess ----------------------------------------

# ベストパラメータで訓練
set.seed(123)
wflw_fit_nnetar_tscv <-
  wflw_tune_nnetar %>%
    finalize_workflow(
        tune_results_nnetar_2 %>% 
            show_best(metric = "rmse", n = Inf) %>%
            dplyr::slice(1)
    ) %>%
    fit(training(splits))

# プロット作成
# --- 時系列の予測結果
wflw_fit_nnetar_tscv %>% calibrate_and_plot()


# 3 PROPHET BOOST  -----------------------------------------------------------------

# [NON-SEQUENTIAL] ----
# - Non-Sequential Model:
#   - 日付を特徴量として使用する
#   - ラグは前処理で外生的に生成する
#   - Spline can be modeled with random missing observations
#   - K-Foldクロスバリデーションでチューニングする
#   - IMPORTANT: Experiment to see which gives better results
# - Other Examples:
#   - Machine Learning Algorithms that use Calendar Features (e.g. GLMNet, XGBoost)
#   - Prophet
# - IMPORTANT: Can use time_series_cv() or vfold_cv(). Usually better performance with vfold_cv().


# * モデル抽出 --------------------------------------------

# モデル抽出
# --- PROPHET W/ XGBOOST ERRORS
wflw_fit_prophet_boost <-
  calibration_tbl %>%
    pluck_modeltime_model(21)


# * Cross Validation Plans (K-Fold) --------------------

# - Prophet is a non-sequential model. We can randomize time observations.
# - K-Fold is OK
# - Should set.seed() because random process

# k-Foldクロスバリデーション
set.seed(123)
resamples_kfold <-
  splits %>%
    training() %>%
    vfold_cv(v = 10)

# プロット作成
resamples_kfold %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans, .facet_ncol = 2)


# * レシピ確認 --------------------------------------------

# データ確認
wflw_fit_prophet_boost %>%
  pull_workflow_preprocessor() %>%
  prep() %>%
  juice() %>%
  glimpse()


# * Model Spec --------------------------------------------

# モデルパラメータの確認
# --- Prophet Boost
wflw_fit_prophet_boost %>% pull_workflow_spec()

# チューニング設定
model_spec_prophet_boost <-
  prophet_boost(changepoint_num    = 25,
                changepoint_range  = 0.8,
                seasonality_yearly = FALSE,
                seasonality_weekly = FALSE,
                seasonality_daily  = FALSE,
                mtry           = tune(),
                trees          = 300,
                min_n          = tune(),
                tree_depth     = tune(),
                learn_rate     = tune(),
                loss_reduction = tune()) %>%
    set_engine("prophet_xgboost")


# * チューニンググリッドの設定 ------------------------------------------

# ラテンハイパーキューブサーチ
# --- mtryのみ設定変更
set.seed(123)
grid_spec_prophet_boost_1 <-
  model_spec_prophet_boost %>%
      parameters() %>%
        update(mtry = mtry(range = c(1, 65))) %>%
        grid_latin_hypercube(size = 15)

# ラテンハイパーキューブサーチ
# --- チューニング対象のパラメータを列挙
set.seed(123)
grid_spec_prophet_boost_2 <-
  grid_latin_hypercube(
    mtry(range = c(1, 65)),
    min_n(),
    tree_depth(),
    learn_rate(range = c(-1.5, -0.8)),
    loss_reduction(),
    size = 15
)

# ラテンハイパーキューブサーチ
# --- チューニング対象のパラメータを列挙
set.seed(123)
grid_spec_prophet_boost_3 <- grid_latin_hypercube(
    mtry(range = c(1, 25)),
    min_n(range = c(3, 20)),
    tree_depth(range = c(3, 10)),
    learn_rate(range = c(-1.5, -0.8)),
    loss_reduction(),
    size = 15
)

# * チューニング ------------------------------------------------------

# ** 並列化の設定 --------------------------------------------

# 並列化の開始
registerDoFuture()

# コア数の取得
n_cores <- parallel::detectCores()

# ヘルプ参照
# ?plan
plan(strategy = cluster,
     workers  = parallel::makeCluster(n_cores))


# ** K-Foldクロスバリデーション ------------------------------

# グリッド設定
# ---- No3を使用
grid <- grid_spec_prophet_boost_3

tic()
set.seed(123)
tune_results_prophet_kfold <-
  wflw_fit_prophet_boost %>%
    update_model(model_spec_prophet_boost) %>%
    tune_grid(resamples = resamples_kfold,
              grid      = grid,
              metrics   = default_forecast_accuracy_metric_set(),
              control   = control_grid(verbose = FALSE, save_pred = TRUE))
toc()


# ** Reset Sequential Plan ---------------------------------

plan(strategy = sequential)



# 結果確認 -----------------------------------------------------

# 上位ランキング
# --- meanが小さい順に出力
tune_results_prophet_kfold %>%
    show_best(metric = "rmse", n = Inf)

# プロット作成
# --- 横軸：パラメータ
# --- 縦軸：メトリック
g <-
  tune_results_prophet_kfold %>%
    autoplot() +
    geom_smooth(se = FALSE)

# プロット更新
# --- インタラクティブ変換
ggplotly(g)


# * Retrain & Assess ----

# ベストパラメータで訓練
set.seed(123)
wflw_fit_prophet_boost_kfold_rmse <-
  wflw_fit_prophet_boost %>%
    update_model(model_spec_prophet_boost) %>%
    finalize_workflow(
        tune_results_prophet_kfold %>%
            show_best(metric = "rmse") %>%
            dplyr::slice(1)
    ) %>%
    fit(training(splits))


# プロット作成
# --- 時系列の予測結果
set.seed(123)
wflw_fit_prophet_boost_kfold_rsq <-
  wflw_fit_prophet_boost %>%
    update_model(model_spec_prophet_boost) %>%
    finalize_workflow(
        tune_results_prophet_kfold %>%
            show_best(metric = "rsq") %>%
            dplyr::slice(1)
    ) %>%
    fit(training(splits))


# プロット作成
# --- 時系列の予測結果
calibrate_and_plot(
    wflw_fit_prophet_boost_kfold_rmse,
    wflw_fit_prophet_boost_kfold_rsq
)


# 4 モデル保存 ----------------------------------------------

# # モデル整理
# set.seed(123)
# calibration_tbl <-
#   modeltime_table(wflw_fit_nnetar_tscv,
#                   wflw_fit_prophet_boost_kfold_rmse,
#                   wflw_fit_prophet_boost_kfold_rsq) %>%
#     modeltime_calibrate(testing(splits), quiet = FALSE)
#
# # モデル保存
# calibration_tbl %>%
#     write_rds("00_models/calibration_tbl_hyperparameter_tuning.rds")
#
# # 予測精度
# # --- 確認のみ
# calibration_tbl %>% modeltime_accuracy()



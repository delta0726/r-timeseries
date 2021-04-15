# *****************************************************************************
# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: ENSEMBLE METHODS
# *****************************************************************************


# SPECIAL REQUIREMENTS:
# - Ensembling: Requires modeltime.ensemble
# - Installation: devtools::install_github("business-science/modeltime.ensemble")

# ＜ゴール＞:
# メタラーナー(アンサンブル学習器)の作り方と優位性を学ぶ


# ＜目的＞
# - 単純平均アンサンブル
# - 加重平均アンサンブル
# - スタッキング・アンサンブル
# - マルチレベル・アンサンブル


# ＜目次＞
# 0 準備
# 1 モデルレビュー
# 2 平均/中央値・アンサンブル
# 3 加重ウエイト・アンサンブル
# 4 スタッキング
# 5 マルチレベル・スタッキング
# 6 MODELTIMEによるフロー
# 7 モデル保存


# 0 準備 -----------------------------------------------------------------------------

# * Change Locale -----------------------------------------------------

Sys.setlocale("LC_TIME", "English")
Sys.getlocale("LC_TIME")


# LIBRARIES & SETUP --------------------------------------------------

# Time Series ML
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)
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


# * 並列処理の設定 ---------------------------------------------------

# 並列処理の開始
registerDoFuture()

# コア数の取得
n_cores <- parallel::detectCores()

# 並列処理の設定
plan(strategy = cluster,
     workers  = parallel::makeCluster(n_cores))

# plan(sequential)


# データ準備 ---------------------------------------------------------------

# データロード
artifacts_list <- read_rds("00_models/feature_engineering_artifacts_list.rds")

# データ取得
data_prepared_tbl <- artifacts_list$data$data_prepared_tbl

# 確認
data_prepared_tbl %>% print()
data_prepared_tbl %>% glimpse()


# モデル準備 --------------------------------------------------------------

# モデルロード
calibration_tune_tbl    <- read_rds("00_models/calibration_tbl_hyperparameter_tuning.rds")
calibration_ml_tbl      <- read_rds("00_models/machine_learning_calibration_tbl.rds")
calibration_boosted_tbl <- read_rds("00_models/calibration_tbl_boosted_models.rds")
calibration_ets_tbl     <- read_rds("00_models/calibration_tbl_ets_tbats.rds")


# データ分割 --------------------------------------------------------------

# データ分割
# --- 時系列分割
splits <-
  data_prepared_tbl %>%
    time_series_split(assess = "8 weeks", cumulative = TRUE)

# プロット作成
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)


# 1 モデルレビュー  -----------------------------------------------------------------------

# * モデルテーブルの結合 ----

# モデルテーブルの結合
model_tbl <-
  combine_modeltime_tables(
    calibration_tune_tbl %>% mutate(.model_desc = .model_desc %>% str_c(" - Tuned")),
    calibration_ml_tbl,
    calibration_boosted_tbl,
    calibration_ets_tbl
)

# 検証データ作成
# --- 訓練データで再学習（REFIT）
# --- 検証データの作成（CALIBRATE）
calibration_tbl <-
  model_tbl %>%
    modeltime_refit(data = training(splits) %>% drop_na()) %>%
    modeltime_calibrate(new_data = testing(splits))


# * サブモデルの選択 ----

# モデル精度の検証
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(defaultPageSize = 40,
                           bordered        = TRUE,
                           resizable       = TRUE)

# モデルIDの取得
# --- Accuracyの上位モデル
# --- サブテーブル作成用
model_id_selection <-
  calibration_tbl %>%
    modeltime_accuracy() %>%
    arrange(rmse) %>%
    slice(1:10) %>%
    slice(-1, -7) %>%
    pull(.model_id)

# モデル抽出
submodels_tbl <-
  calibration_tbl %>%
    filter(.model_id %in% model_id_selection)

# 確認
submodels_tbl %>% print()


# 2 平均/中央値・アンサンブル -----------------------------------------------

# ＜ポイント＞
# - コンセプト:
#   - Use sub-model predictions as inputs, take a simple average (or median)
# - 長所:
#   - 素早く構築することができる (追加的なトレーニングが必要ない)
# - 短所:
#   - 予測精度の悪いモデルの影響が大きくなる可能性がある (中央値の場合は緩和される)


# ヘルプ参照
?ensemble_average


# * アンサンブルモデルの構築（平均） ----------------------------------------

# アンサンブルモデルの作成
ensemble_fit_mean <-
  submodels_tbl %>%
    ensemble_average(type = "mean")

# オブジェクト構造
# --- tibbleに属性データが付いただけ
# --- この時点ではアンサンブルの計算はされていない
ensemble_fit_mean %>% glimpse()

# モデル精度の確認
modeltime_table(ensemble_fit_mean) %>%
  modeltime_accuracy(testing(splits))


# * アンサンブルモデルの構築（中央値） ----------------------------------------

# アンサンブルモデルの作成
ensemble_fit_median <-
  submodels_tbl %>%
    ensemble_average("median")

# オブジェクト構造
# --- tibbleに属性データが付いただけ
# --- この時点ではアンサンブルの計算はされていない
ensemble_fit_median %>% glimpse()

# モデル精度の確認
modeltime_table(ensemble_fit_mean,
                ensemble_fit_median) %>%
  modeltime_accuracy(testing(splits))


# 3 加重ウエイト・アンサンブル ----------------------------------------------

# - 長所:
#   - 単純平均よりも改善余地が残されている
#   - 素早く構築することができる (追加的なトレーニングが必要ない)
# - 短所:
#   - ウエイトを決める方法を検討する必要がある
# - Technique: Use a simple ranking (fastest)


# ヘルプ参照
?ensemble_weighted

# RMSEのランキング
# --- ランキングをウエイトにする
loadings_tbl <-
  submodels_tbl %>%
    modeltime_accuracy() %>%
    mutate(rank = min_rank(-rmse)) %>%
    select(.model_id, rank)

# アンサンブルモデルの作成
ensemble_fit_wt <-
  submodels_tbl %>%
    ensemble_weighted(loadings = loadings_tbl$rank)

# オブジェクト構造
# --- モデルごとのウエイトが管理されている
# --- この時点ではアンサンブルの計算はされていない
ensemble_fit_wt %>% glimpse()
ensemble_fit_wt$fit$loadings_tbl

# ウエイトの確認
ensemble_fit_wt$fit$loadings_tbl

# モデル精度の確認
# --- メタラーナー（サブモデルではない点に注意）
modeltime_table(ensemble_fit_mean,
                ensemble_fit_median,
                ensemble_fit_wt) %>%
    modeltime_accuracy(testing(splits))


# 4 スタッキング ----------------------------------------------------------------

# - コンセプト:
#   - We use sub-model predictions as inputs; use a meta-learner to predict
# - Penalized Regression Technique: Assigns loadings (coefficients) to a
#     Elastic Net Model (glmnet). Those loadings are weights that can be used
#     as a simple weighted prediction (this is what glmnet does).
# - 長所:
#   - This de-weights bad models, improving predictive accuracy
# - 短所:
#   - More expensive operation when using Stacking with Cross Validation.

# ヘルプ参照
?ensemble_model_spec


# * 4.1 リサンプリング ---------------------------------------------

# - Step 1: modeltime_fit_resamples() -------------------

# *** 時系列クロスバリデーション ----------------

# データ分割
# --- Foldは2つのみ
resamples_tscv <-
  splits %>%
    training() %>%
    drop_na() %>%
    time_series_cv(assess     = "8 weeks",
                   skip       = "4 weeks",
                   initial    = "12 months",
                   cumulative = TRUE)

# プロット作成
# --- 訓練データ / 検証データ
resamples_tscv %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)

# クロスバリデーション
# --- 検証データの作成
# --- 22.09 sec elapsed
submodels_resamples_tscv_tbl <-
  submodels_tbl %>%
    modeltime_fit_resamples(resamples = resamples_tscv,
                            control   = control_resamples(verbose   = TRUE,
                                                          allow_par = TRUE,
                                                          pkgs      = c("Cubist", "rules"))
    )

# 予測値の確認
# --- 検証データ
# --- ｢.resample_results｣列の確認
submodels_resamples_tscv_tbl$.resample_results[[1]]
submodels_resamples_tscv_tbl$.resample_results[[1]]$.predictions


# *** K-FOLDクロスバリデーション ----

# データ分割
# --- Foldは10個
set.seed(123)
resamples_kfold <-
  splits %>%
    training() %>%
    drop_na() %>%
    vfold_cv(v = 10)

# プロット作成
# --- 訓練データ / 検証データ
resamples_kfold %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans, .facet_ncol = 2)

# クロスバリデーション
# --- 検証データの作成
# --- 51.02 sec elapsed
submodels_resamples_kfold_tbl <-
  submodels_tbl %>%
    dplyr::slice(-1) %>%
    modeltime_fit_resamples(resamples = resamples_kfold,
                            control   = control_resamples(verbose    = TRUE,
                                                          allow_par  = TRUE,
                                                          pkgs       = c("Cubist", "rules"))
    )

# 予測値の取得
submodels_resamples_kfold_tbl$.resample_results[[1]]
submodels_resamples_kfold_tbl$.resample_results[[1]]$.predictions


# * 4.2 スタッキング --------------------------------------------------

# ***  LM STACK -------------------------------

# 時系列クロスバリデーション
# --- 線形回帰
set.seed(123)
ensemble_fit_lm_tscv <-
  submodels_resamples_tscv_tbl %>%
    ensemble_model_spec(model_spec = linear_reg() %>% set_engine("lm"),
                        control    = control_grid(verbose = TRUE))

# モデル精度の検証
modeltime_table(ensemble_fit_lm_tscv) %>%
  modeltime_accuracy(testing(splits))


# K-FOLDクロスバリデーション
# --- 線形回帰
set.seed(123)
ensemble_fit_lm_kfold <-
  submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(model_spec = linear_reg() %>% set_engine("lm"),
                        control    = control_grid(verbose = TRUE))

# モデル精度の検証
modeltime_table(ensemble_fit_lm_kfold) %>%
  modeltime_accuracy(testing(splits))


# *** GLMNET STACK -------------------------------

# ＜ポイント＞
# - GLMNETはシーケンシャルモデルではない
#   --- K-FOLDクロスバリデーションのみ


# K-FOLDクロスバリデーション
set.seed(123)
ensemble_fit_glmnet_kfold <-
  submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
      model_spec =
        linear_reg(penalty = tune(), mixture = tune()) %>%
          set_engine("glmnet"),
        kfolds  = 10,
        grid    = 10,
        control = control_grid(verbose   = TRUE,
                               allow_par = TRUE)
    )

# モデル精度の検証
modeltime_table(ensemble_fit_glmnet_kfold) %>%
  modeltime_accuracy(testing(splits))


# *** RANDOM FOREST STACK -------------------------------

# ＜ポイント＞
# - RANDOM FORESTはシーケンシャルモデルではない
#   --- K-FOLDクロスバリデーションのみ

# K-FOLDクロスバリデーション
# --- 線形回帰
set.seed(123)
ensemble_fit_ranger_kfold <-
  submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec =
          rand_forest(trees = tune(),
                       min_n = tune()) %>%
            set_engine("ranger"),
        kfolds  = 10,
        grid    = 10,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )

# モデル精度の検証
modeltime_table(ensemble_fit_ranger_kfold) %>%
  modeltime_accuracy(testing(splits))


# *** NNET STACK -------------------------------

set.seed(123)
ensemble_fit_nnet_kfold <-
  submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = mlp(
            hidden_units = tune(),
            penalty      = tune(),
            epochs       = tune()
        ) %>% set_engine("nnet"),
        kfolds = 10,
        grid   = 10,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )

# モデル精度の検証
modeltime_table(ensemble_fit_nnet_kfold) %>%
  modeltime_accuracy(testing(splits))


# *** XGBOOST STACK -------------------------------

set.seed(123)
ensemble_fit_xgboost_kfold <-
  submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = boost_tree(
            trees          = tune(),
            tree_depth     = tune(),
            learn_rate     = tune(),
            loss_reduction = tune()
        ) %>%
            set_engine("xgboost"),
        kfolds = 10,
        grid   = 10,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )

# モデル精度の検証
modeltime_table(ensemble_fit_xgboost_kfold) %>%
  modeltime_accuracy(testing(splits))


# *** CUBIST STACK -------------------------------

set.seed(123)
ensemble_fit_cubist_kfold <-
  submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = cubist_rules(
            committees = tune(),
            neighbors  = tune(),
            max_rules  = tune()
        ) %>%
            set_engine("Cubist"),
        kfold = 10,
        grid  = 10,
        control = control_grid(
            verbose = TRUE,
            allow_par = TRUE
        )
    )

# モデル精度の検証
modeltime_table(ensemble_fit_cubist_kfold) %>%
  modeltime_accuracy(testing(splits))


# *** SVM STACK -------------------------------

set.seed(123)
ensemble_fit_svm_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = svm_rbf(
            mode      = "regression",
            cost      = tune(),
            rbf_sigma = tune(),  
            margin    = tune()
        ) %>%
            set_engine("kernlab"),
        kfold = 10, 
        grid  = 10, 
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )

# モデル精度の検証
modeltime_table(ensemble_fit_svm_kfold) %>%
  modeltime_accuracy(testing(splits))


# 5 マルチレベル・スタッキング ------------------------------------------------------------

#
model_stack_level_2_accuracy_tbl <-
  modeltime_table(ensemble_fit_glmnet_kfold,
                  ensemble_fit_ranger_kfold,
                  ensemble_fit_nnet_kfold,
                  ensemble_fit_xgboost_kfold,
                  ensemble_fit_cubist_kfold,
                  ensemble_fit_svm_kfold) %>%
    modeltime_accuracy(testing(splits))

# モデル精度の検証
model_stack_level_2_accuracy_tbl %>%
  table_modeltime_accuracy()

# 加重ウエイト・スタッキング
model_stack_level_3_tbl <-
  modeltime_table(ensemble_fit_ranger_kfold,
                  ensemble_fit_svm_kfold,
                  ensemble_fit_glmnet_kfold) %>%
    ensemble_weighted(loadings = c(5, 3, 1)) %>%
    modeltime_table()

# モデル精度の検証
model_stack_level_3_tbl %>%
    modeltime_accuracy(testing(splits))


# 6 MODELTIMEによるフロー ----------------------------------------------------------------

# * Calibration  ----------------------------------------------------

# 検証データの作成
calibration_ensemble_tbl <-
  model_stack_level_3_tbl %>%
    modeltime_calibrate(testing(splits))

# 確認
calibration_ensemble_tbl


# * Accuracy --------------------------------------------------------

# モデル精度の検証
calibration_ensemble_tbl %>% modeltime_accuracy()


# * Test Forecast ----------------------------------------------------

# 予測データの作成
forecast_test_tbl <-
  calibration_ensemble_tbl %>%
    modeltime_forecast(new_data    = testing(splits),
                       actual_data = data_prepared_tbl)

# プロット作成
forecast_test_tbl %>%
    plot_modeltime_forecast()


# * Refit Forecast ---------------------------------------------------

# Not updating the super learner / updating submodels only 

# 学習器の更新
refit_ensemble_submodel_tbl <-
  calibration_ensemble_tbl %>%
    modeltime_refit(data = data_prepared_tbl)

# 予測データの作成
forecast_submodels_tbl <-
  refit_ensemble_submodel_tbl %>%
    modeltime_forecast(new_data    = artifacts_list$data$forecast_tbl,
                       actual_data = data_prepared_tbl)

# プロット作成
forecast_submodels_tbl %>%
    plot_modeltime_forecast()

# 学習器の更新
# Updating the superlearner and the submodels
set.seed(123)
refit_ensemble_superlearner_tbl <-
  calibration_ensemble_tbl %>%
    modeltime_refit(
        data = data_prepared_tbl,
        resamples = data_prepared_tbl %>%
            drop_na() %>%
            vfold_cv(v = 10)
    )

# 予測データの作成
forecast_superlearner_tbl <-
  refit_ensemble_superlearner_tbl %>%
    modeltime_forecast(new_data    = artifacts_list$data$forecast_tbl,
                       actual_data = data_prepared_tbl)

# プロット作成
forecast_superlearner_tbl %>%
    plot_modeltime_forecast()


# * 並列処理の終了 ----

plan(sequential)


# 7 モデル保存 ----------------------------------------------------------------------------

# モデル保存
#model_stack_level_3_tbl %>%
#    write_rds("00_models/model_stack_level_3_tbl.rds")

# モデル確認
#model_stack_level_3_tbl$.model[[1]]$model_tbl$.model[[2]]$model_tbl

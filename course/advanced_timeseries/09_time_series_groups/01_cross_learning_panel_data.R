# ******************************************************************************
# BUSINESS SCIENCE UNIVERSITY ----
# DS4B 203-R: TIME SERIES FORECASTING  ----
# MODULE: SCALABLE TIME SERIES - CROSS-SECTIONAL LEARNING ----
# ******************************************************************************


# ゴール ----
# グループで区分された時系列データを一括で予測する


# 目的 ----
# - Cross-Sectional Learning - Forecast Grouped Data using Cross-Sections
# - Panel Data - Become comfortable with Overlapping Time Stamps
# - Time Series Resampling - Evaluating Model Stability Over Time
# - Ensembling - Multiple Cross-Sectional Models


# IMPORTANT ----
# - これらの手法は非シーケンシャルモデルのみ(機械学習など)で使用することができる
#   --- ARIMA, NNETAR, TBATS などでは使えない


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 時系列データ分割
# 3 レシピ作成
# 4 モデル構築
# 5 ハイパーパラメータのチューニング
# 6 パネル予測の評価
# 7 時系列クロスバリデーション
# 8 パネルモデルのアンサンブル
# 9 まとめ


# 0 準備 -------------------------------------------------------------------------------

# ** ライブラリのロード ------------------------------------------------

# Time Series ML
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)

# Timing & Parallel Processing
library(tictoc)
library(future)
library(doFuture)

# Core 
library(tidyquant)
library(tidyverse)
library(timetk)


# ** 並列設定 --------------------------------------------------------

# 並列処理の開始
registerDoFuture()

# コア数の取得
n_cores <- parallel::detectCores()

# 並列化の設定
plan(strategy = cluster,
     workers  = parallel::makeCluster(n_cores))

# plan(sequential)


# ** データ準備 ----------------------------------------------------

# データロード
# --- pagePathごとの時系列データ
ga_page_raw_tbl <- read_rds("00_data/google_analytics_by_page_daily.rds")

# データ確認
# --- データ開始日: 2019-05-08
# --- データ終了日: 2020-03-02
ga_page_raw_tbl %>% tk_index() %>% tk_get_timeseries_summary()

# データ確認
# --- ページごとのデータ開始日
ga_page_raw_tbl %>%
  select(date, pagePath) %>%
  group_by(pagePath) %>%
  summarise(SD = first(date),
            ED = last(date)) %>%
  ungroup()

# プロット作成
# --- 個別の時系列データごと
ga_page_raw_tbl %>%
  group_by(pagePath) %>%
  plot_time_series(date, pageViews,
                   .facet_ncol  = 4,
                   .smooth      = FALSE,
                   .interactive = TRUE)


# 1 データ加工 --------------------------------------------------------------------

# ** 全期間データ(加工済データ) ------------------------------------------------

# データ加工
full_data_tbl <-
  ga_page_raw_tbl %>%
    # 存在しない日付レコードの補完
    # --- 前後にデータが存在する系列のみ
    select(date, pagePath, pageViews) %>%
    group_by(pagePath) %>%
    pad_by_time(date, .by = "day", .pad_value = 0) %>%
    ungroup() %>%
    # 対数変換
    mutate(pageViews = log1p(pageViews)) %>%
    # グループごとに将来日付を追加（日付とグループキー以外はNA）
    group_by(pagePath) %>%
    future_frame(date, .length_out = 28, .bind_data = TRUE) %>%
    ungroup() %>%
    # 系列追加
    # --- （フーリエ / ラグ / 移動平均）
    mutate(pagePath = as_factor(pagePath)) %>%
    group_by(pagePath) %>%
    group_split() %>%
    map(.f = function(df) {
        df %>%
            arrange(date) %>%
            tk_augment_fourier(date, .periods = c(14, 28)) %>%
            tk_augment_lags(pageViews, .lags = 28) %>%
            tk_augment_slidify(
                pageViews_lag28,
                .f       = ~ mean(.x, na.rm = TRUE),
                .period  = c(7, 28, 28*2),
                .partial = TRUE, 
                .align   = "center"
            )
    }) %>%
    bind_rows() %>%
    rowid_to_column(var = "rowid")

# データ確認
full_data_tbl %>% print()
full_data_tbl %>% glimpse()

# データ確認
# --- データ開始日: 2019-05-08
# --- データ終了日: 2020-03-30（03-02から28日分の日付が追加されている）
full_data_tbl %>% tk_index() %>% tk_get_timeseries_summary()

# データ確認
# --- ページごとのデータ開始日
full_data_tbl %>%
  select(date, pagePath) %>%
  group_by(pagePath) %>%
  summarise(SD = first(date),
            ED = last(date)) %>%
  ungroup()

# プロット作成
# --- 途中からデータが開始される系列も存在する
full_data_tbl %>%
  group_by(date) %>%
  tally() %>%
  plot_time_series(date, n)


# ** モデル構築用データ -----------------------------------------

# モデル構築用データ
# --- 将来データを削除（pageViewsがNA）
# --- 移動平均計算などでNAとなった最初のほうの系列を削除
data_prepared_tbl <-
  full_data_tbl %>%
    filter(!is.na(pageViews)) %>%
    drop_na()

# 確認
data_prepared_tbl %>% print()


# ** 将来データ -------------------------------------------------

# 将来データ
# --- 将来データを選択（pageViewsがNA）
future_tbl <-
  full_data_tbl %>%
    filter(is.na(pageViews))

# データ確認
# --- nan(非数)が存在する
future_tbl %>% filter(is.nan(pageViews_lag28_roll_28))

# データ変換
# --- nanを一度NAに変換してから0に置換する
# --- ビデオではreplace_na()の処理の代わりに次のコードを使っている
# --- fill(contains("_lag"), .direction = "up")
future_tbl <-
  future_tbl %>%
    mutate(across(.cols = contains("_lag"),
                  .fns  = function(x) ifelse(is.nan(x), NA, x))) %>%
    mutate(across(.cols = contains("_lag"), .fns = ~replace_na(.x, 0)))

# データ確認
# --- nan(非数)がなくなった
future_tbl %>% filter(is.na(pageViews_lag28_roll_28))


# 2 時系列データ分割 -------------------------------------------------------------------------

# 時系列データ分割
splits <-
  data_prepared_tbl %>%
    time_series_split(date, assess = 28, cumulative = TRUE)


# プロット作成
# --- tk_time_series_cv_plan()で｢.id｣｢.key｣が追加されている
# --- 元のスクリプトは全データ系列を表示していたが、見やすくするためルートディレクトリのみ抽出
splits %>%
  tk_time_series_cv_plan() %>%
  filter(pagePath == "/") %>%
  plot_time_series_cv_plan(date, pageViews)


# 3 レシピ作成 -----------------------------------------------------------------------------

# ** 異常値処理 ----------------------------------------------------

# ＜ポイント＞
# - レシピに投入するデータセットの段階でクリーニングしておく必要がある
# - グループ化して系列ごとに行う

# 時系列データのクリーニング
# --- ロバストSTL分解と線形補完を用いて季節調整後データを得る
# --- 異常値が小さくなる
train_cleaned <-
  splits %>%
    training() %>%
    group_by(pagePath) %>%
    mutate(pageViews = ts_clean_vec(pageViews, period = 7)) %>%
    ungroup()

# プロット作成
# --- クリーニング前
splits %>%
  training() %>%
  group_by(pagePath) %>%
  plot_time_series(date, pageViews,
                   .facet_ncol  = 4,
                   .smooth      = FALSE,
                   .interactive = TRUE)

# プロット作成
# --- クリーニング後
train_cleaned %>%
  group_by(pagePath) %>%
  plot_time_series(date, pageViews,
                   .facet_ncol  = 4,
                   .smooth      = FALSE,
                   .interactive = TRUE)


# ** レシピ作成 ------------------------------------------------

# レシピ作成
# --- モデリング/予測の処理でrowidが削除されないようにロールを割り当てておく
recipe_spec <-
  recipe(pageViews ~ ., data = train_cleaned) %>%
    update_role(rowid, new_role = "indicator") %>%
    step_timeseries_signature(date) %>%
    step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)")) %>%
    step_normalize(date_index.num, date_year) %>%
    step_other(pagePath) %>%
    step_dummy(all_nominal(), one_hot = TRUE)

# データ出力
recipe_spec %>% prep() %>% juice() %>% glimpse()

# ロール確認
# ---dateがpredictorに含まれている
recipe_spec %>% summary()


# 4 モデル構築 ----------------------------------------------------------------------------

# ＜ポイント＞
# - ワークフローにモデルとレシピをセット

# ＜注意事項＞
# - これらの手法は非シーケンシャルモデル(機械学習など)でのみ使用することができる点に注意
#   --- ARIMA, NNETAR, TBATS などでは使えない


# ** PROPHET ----------------------------------------------------

wflw_fit_prophet <-
  workflow() %>%
    add_model(spec = prophet_reg() %>% set_engine("prophet")) %>%
    add_recipe(recipe_spec) %>%
    fit(train_cleaned)


# ** XGBOOST ----------------------------------------------------

wflw_fit_xgboost <-
  workflow() %>%
    add_model(spec = boost_tree(mode = "regression") %>% set_engine("xgboost")) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>%
    fit(train_cleaned)


# ** PROPHET BOOST ----------------------------------------------

wflw_fit_prophet_boost <-
  workflow() %>%
    add_model(spec = prophet_boost(seasonality_daily  = FALSE,
                                   seasonality_weekly = FALSE,
                                   seasonality_yearly = FALSE) %>%
                    set_engine("prophet_xgboost")) %>%
    add_recipe(recipe_spec) %>%
    fit(train_cleaned)


# ** SVM --------------------------------------------------------

wflw_fit_svm <-
  workflow() %>%
    add_model(spec = svm_rbf(mode = "regression") %>% set_engine("kernlab")) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>%
    fit(train_cleaned)


# ** RANDOM FOREST -----------------------------------------------

wflw_fit_rf <-
  workflow() %>%
    add_model(spec = rand_forest(mode = "regression") %>% set_engine("ranger")) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>%
    fit(train_cleaned)


# ** NNET --------------------------------------------------------

wflw_fit_nnet <-
  workflow() %>%
    add_model(spec = mlp(mode = "regression") %>% set_engine("nnet")) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>%
    fit(train_cleaned)


# ** MARS -------------------------------------------------------

wflw_fit_mars <-
  workflow() %>%
    add_model(spec = mars(mode = "regression") %>% set_engine("earth")) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator")) %>%
    fit(train_cleaned)


# ** ACCURACY CHECK ---------------------------------------------

# モデルテーブルの作成
# --- 学習済モデルを投入
submodels_1_tbl <-
  modeltime_table(wflw_fit_prophet,
                  wflw_fit_xgboost,
                  wflw_fit_prophet_boost,
                  wflw_fit_svm,
                  wflw_fit_rf,
                  wflw_fit_nnet,
                  wflw_fit_mars)

# 確認
submodels_1_tbl %>% print()

# Accuracyの確認
# --- モデル精度のファーストインプレッションを得る
submodels_1_tbl %>%
  modeltime_accuracy(testing(splits)) %>%
  arrange(rmse)


# 5 ハイパーパラメータのチューニング --------------------------------------------------------

# ＜ポイント＞
# - ｢XGBoost｣｢ランダムフォレスト｣｢MARS｣の3モデルについてチューニングを実行
#   --- チューニングは計算コストが高い処理なので、ざっくり例を示している程度

# ＜注意事項＞
# - ここでは時系列パネルデータとして特段の工夫は行わずにFoldを作成する
#   --- Foldごとの訓練データで系列(pagePath)に対する指定はない
#   --- 通常の機械学習のとおり


# ** リサンプリング - K-FOLD ----------------------------------------

# リサンプリング作成
# --- 訓練データから5Foldを作成
# --- クロスセクション分割
set.seed(123)
resamples_kfold <- train_cleaned %>% vfold_cv(v = 5)

# プロット作成
# --- クロスセクションのデータ分割
resamples_kfold %>%
  tk_time_series_cv_plan() %>%
  #filter(pagePath == "/") %>%
  plot_time_series_cv_plan(date, pageViews, .facet_ncol = 2)

# 参考： Fold1の確認
# --- 訓練データ
resamples_kfold$splits[[1]] %>%
  training() %>%
  filter(pagePath == "/") %>%
  group_by(date) %>%
  tally()

# --- 検証データ
resamples_kfold$splits[[1]] %>%
  testing() %>%
  filter(pagePath == "/") %>%
  group_by(date) %>%
  tally()


# ** チューニング： XGBOOST  ----------------------------------------

# *** 準備

# モデル構築
# --- チューニング用
model_spec_xgboost_tune <-
  boost_tree(mode            = "regression",
             mtry            = tune(),
             trees           = tune(),
             min_n           = tune(),
             tree_depth      = tune(),
             learn_rate      = tune(),
             loss_reduction  = tune()) %>%
    set_engine("xgboost")

# ワークフロー設定
wflw_spec_xgboost_tune <-
  workflow() %>%
    add_model(model_spec_xgboost_tune) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))

# パラメータ準備
param_info <-
  wflw_spec_xgboost_tune %>%
    parameters() %>%
    update(learn_rate = learn_rate(range = c(0.001, 0.400), trans = NULL))


# *** チューニング

# 前処理
tic()
set.seed(123)

# グリッドサーチ実行
# --- learn_rate(学習率)のみチューニング範囲を変更
# --- 56.17 sec elapsed
tune_results_xgboost <-
  wflw_spec_xgboost_tune %>%
    tune_grid(resamples  = resamples_kfold,
              param_info = param_info,
              grid = 10,
              control = control_grid(verbose = TRUE, allow_par = TRUE))

# 後処理
toc()


# *** 結果確認

# 最良パラメータの確認
# --- RMSEが小さい順から組み合わせを表示
tune_results_xgboost %>% show_best("rmse", n = Inf)


# *** モデル適用

# 最良モデルで学習
# --- チューニング用に作成したモデルを更新する
wflw_fit_xgboost_tuned <-
  wflw_spec_xgboost_tune %>%
    finalize_workflow(select_best(tune_results_xgboost, "rmse")) %>%
    fit(train_cleaned)


# ** チューニング： RANGER ----------------------------------------

# *** 準備

# モデル構築（チューニング用）
model_spec_rf_tune <-
  rand_forest(mode    = "regression",
              mtry    = tune(),
              trees   = tune(),
              min_n   = tune()) %>%
    set_engine("ranger")

# ワークフロー設定
wflw_spec_rf_tune <-
  workflow() %>%
    add_model(model_spec_rf_tune) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))


# *** チューニング

# 前処理
tic()
set.seed(123)

# グリッドサーチ実行
tune_results_rf <-
  wflw_spec_rf_tune %>%
    tune_grid(resamples = resamples_kfold,
              grid      = 5,
              control   = control_grid(verbose = TRUE, allow_par = TRUE))

# 後処理
toc()


# *** 結果確認

# 最良パラメータの確認
# --- RMSEが小さい順から組み合わせを表示
tune_results_rf %>% show_best("rmse", n = Inf)


# *** モデル適用

# 最良モデルで学習
# --- チューニング用に作成したモデルを更新する
wflw_fit_rf_tuned <-
  wflw_spec_rf_tune %>%
    finalize_workflow(select_best(tune_results_rf, "rmse")) %>%
    fit(train_cleaned)


# ** チューニング： EARTH --------------------------------------------

# *** 準備

# モデル構築（チューニング用）
model_spec_earth_tune <-
  mars(mode        = "regression",
       num_terms   = tune(),
       prod_degree = tune()) %>%
    set_engine("earth")

# ワークフロー設定
wflw_spec_earth_tune <-
  workflow() %>%
    add_model(model_spec_earth_tune) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "indicator"))


# *** チューニング

# 前処理
tic()
set.seed(123)

# グリッドサーチ実行
tune_results_earth <-
  wflw_spec_earth_tune %>%
    tune_grid(resamples = resamples_kfold,
              grid      = 10,
              control   = control_grid(allow_par = TRUE, verbose = TRUE))

# 後処理
toc()


# *** 結果確認

# 最良パラメータの確認
# --- RMSEが小さい順から組み合わせを表示
tune_results_earth %>% show_best("rmse")


# *** モデル適用

# 最良モデルで学習
# --- チューニング用に作成したモデルを更新する
wflw_fit_earth_tuned <-
  wflw_spec_earth_tune %>%
    finalize_workflow(tune_results_earth %>% select_best("rmse")) %>%
    fit(train_cleaned)



# 6 パネル予測の評価  -----------------------------------------------------------------

# ** Model Table ------------------------------------------------

# モデルテーブルの作成
# --- チューニング済モデル
# --- チューニングなしのモデルテーブルを結合
submodels_2_tbl <-
  modeltime_table(wflw_fit_xgboost_tuned,
                  wflw_fit_rf_tuned,
                  wflw_fit_earth_tuned) %>%
    update_model_description(1, "XGBOOST - Tuned") %>%
    update_model_description(2, "RANGER - Tuned") %>%
    update_model_description(3, "EARTH - Tuned") %>%
    combine_modeltime_tables(submodels_1_tbl)


# ** Calibration ------------------------------------------------

# 検証データの作成
# --- 学習済モデル自体の操作は行っていない
calibration_tbl <-
  submodels_2_tbl %>%
    modeltime_calibrate(testing(splits))


# ** Accuracy ------------------------------------------------

# モデル評価
# --- 検証データでRMSEを用いて評価
calibration_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse)


# ** Forecast Test --------------------------------------------

# プロット作成
# --- テスト期間の予測値を作成
calibration_tbl %>%
  modeltime_forecast(new_data    = testing(splits),
                     actual_data = data_prepared_tbl,
                     keep_data   = TRUE) %>%
  group_by(pagePath) %>%
  plot_modeltime_forecast(.facet_ncol         = 4,
                          .conf_interval_show = FALSE,
                          .interactive        = TRUE)


# 検証： レコード数
# --- pagePathを｢/｣のみ抽出
# --- ACTUAL ： 構築期間
# --- モデル名： 予測期間
calibration_tbl %>%
  modeltime_forecast(new_data    = testing(splits),
                     actual_data = data_prepared_tbl,
                     keep_data   = TRUE) %>%
  filter(pagePath == "/") %>%
  group_by(.model_desc) %>%
  tally() %>%
  print(n = nrow(.))


# 7 時系列クロスバリデーション ---------------------------------------------------------------------

# ＜ポイント＞
# - 時系列クロスバリデーションの実行
#   --- 訓練データと評価データに時系列的な関係を与える
# - Helps us strategize an ensemble approach


# ** 時系列CVデータセットの作成 ----------------------------------------

# データ分割
resamples_tscv <-
  train_cleaned %>%
    time_series_cv(assess      = 28,
                   skip        = 28,
                   cumulative  = TRUE,
                   slice_limit = 4)

# プロット作成
# --- 直近が評価データになる
resamples_tscv %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, pageViews)


# ** リサンプリング予測 ------------------------------------------------

# リサンプリング予測
# --- モデル精度の評価
# --- モデルテーブルに登録された全てのモデルに対して行う
model_tbl_tuned_resamples <-
  submodels_2_tbl %>%
    modeltime_fit_resamples(resamples = resamples_tscv,
                            control   = control_resamples(verbose = TRUE,
                                                          allow_par = TRUE))


# ** Accuracyの算出 ------------------------------------------------

# Accuracyの算出
model_tbl_tuned_resamples %>%
    modeltime_resample_accuracy(metric_set  = metric_set(rmse, rsq),
                                summary_fns = list(mean = mean, sd = sd)) %>%
    arrange(rmse_mean)


# ** Resampling Accuracy Plot ------------------------------------

# プロット作成
# --- モデル精度の評価
model_tbl_tuned_resamples %>%
    plot_modeltime_resamples(.metric_set  = metric_set(mae, rmse, rsq),
                             .point_size  = 4,
                             .point_alpha = 0.8,
                             .facet_ncol  = 1)


# 8 パネルモデルのアンサンブル ---------------------------------------------------------

# ** Average Ensemble --------------------------------------------------

# アンサンブル対象の指定
# --- .model_id
submodels_2_ids_to_keep <- c(1, 4, 6, 2)

# アンサンブルモデルの作成
# --- 指定モデルの中央値
model_ensemble_tbl <-
  submodels_2_tbl %>%
    filter(.model_id %in% submodels_2_ids_to_keep) %>%
    ensemble_average(type = "median") %>%
    modeltime_table()


# ** モデル評価 ----------------------------------------------------------

# 予測精度の評価
# --- アンサンブルモデル
model_ensemble_tbl %>%
    modeltime_accuracy(testing(splits))


# ** 予測 ----------------------------------------------------------------

# 予測データの作成
forecast_ensemble_test_tbl <-
  model_ensemble_tbl %>%
    modeltime_forecast(new_data    = testing(splits),
                       actual_data = data_prepared_tbl,
                       keep_data   = TRUE) %>%
    mutate(across(.cols = c(.value, pageViews), .fns = expm1))

# プロット作成
# --- 予測データを表示
forecast_ensemble_test_tbl %>%
    group_by(pagePath) %>%
    plot_modeltime_forecast(.facet_ncol = 4)

# プロット作成
# --- モデル精度の評価
forecast_ensemble_test_tbl %>%
    filter(.key == "prediction") %>%
    select(pagePath, .value, pageViews) %>%
    # group_by(pagePath) %>%
    summarize_accuracy_metrics(truth      = pageViews,
                               estimate   = .value,
                               metric_set = metric_set(mae, rmse, rsq))


# ** Refit ----------------------------------------------------------------

# 検証データの再作成
data_prepared_tbl_cleaned <-
  data_prepared_tbl %>%
    group_by(pagePath) %>%
    mutate(pageViews = ts_clean_vec(pageViews, period = 7)) %>%
    ungroup()

# モデル再学習
model_ensemble_refit_tbl <-
  model_ensemble_tbl %>%
    modeltime_refit(data_prepared_tbl_cleaned)

# プロット作成
model_ensemble_refit_tbl %>%
    modeltime_forecast(new_data    = future_tbl,
                       actual_data = data_prepared_tbl,
                       keep_data   = TRUE) %>%
    mutate(.value    = expm1(.value),
           pageViews = expm1(pageViews)) %>%
    group_by(pagePath) %>%
    plot_modeltime_forecast(.facet_ncol   = 4,
                            .y_intercept  = 0)

# ** Turn OFF Parallel Backend
plan(sequential)


# 9 まとめ ------------------------------------------------------------------------

# - You:
#     1. Prepared 20 Time Series Groups
#     2. Modeled Panel Data
#     3. Hyper Parameter Tuned 
#     4. Resampled & Evaluated Accuracy Over Time
#     5. Ensembled the models using a strategy based on resample stability
#     6. RMSE 143, MAE 46, RSQ 0.40
# - This code can work for 10,000 Time Series. 
#     1. Only expense is Hyper Parameter Tuning
#     2. Watch Saving Ensembles & Models - Memory Size 

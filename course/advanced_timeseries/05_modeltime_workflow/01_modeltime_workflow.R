# ****************************************************************************
# Title       : BUSINESS SCIENCE UNIVERSITY
# Course      : DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# Module      : Modeltime Workflow（7-1 to 7-7）
# Created by  : Owner
# Last Update : 2021/6/26
# URL         : https://university.business-science.io/
# ****************************************************************************


# ＜目的＞
# - Modeltimeのワークフローを理解する
# - CalibrationやRefitの概念を理解する
# - モデル精度のメトリックを理解する
# - 予測ホライズンと信頼区間を理解する
# - 何故refitをするかを理解する


# ＜目次＞
# 0 準備
# 1 モデル構築
# 2 モデルテーブルの構築
# 3 検証準備（Calibration）
# 4 モデル精度の検証（Test Accuracy）
# 5 予測結果の確認（Test Forecast）
# 6 リフィッティング(Refitting)
# 7 最終予測の作成（Final Forecast）


# 0 準備 ------------------------------------------------------------------------------------

# * ライブラリ -----------------------------------------------------------

# Time Series ML
library(tidymodels)
library(modeltime)

# Core
library(tidyverse)
library(timetk)
library(lubridate)
library(magrittr)


# * データロード ---------------------------------------------------------

# データ準備
feature_engineering_artifacts_list <-
  read_rds("00_models/feature_engineering_artifacts_list.rds")

# データ取得
data_prepared_tbl <- feature_engineering_artifacts_list$data$data_prepared_tbl
forecast_tbl      <- feature_engineering_artifacts_list$data$forecast_tbl

# データ確認
# --- 1系列の時系列データ（optins_trans）
# --- 前処理で説明変数を生成している
data_prepared_tbl %>% glimpse()
forecast_tbl %>% glimpse()

# レシピ取得
recipe_spec_2_lag <- feature_engineering_artifacts_list$recipes$recipe_spec_2


# * データ分割 ----------------------------------------------------------

# 時系列データ分割
# --- 直近がテストデータとなるように分割
splits <-
  data_prepared_tbl %>%
    time_series_split(date_var = optin_time,
                      assess = "8 weeks",
                      # initial = "1 year 1 months"
                      cumulative = TRUE)

# 期間確認
splits %>% training() %>% tk_index() %>% tk_get_timeseries_summary()
splits %>% testing() %>% tk_index() %>% tk_get_timeseries_summary()

# プロット作成
# --- 訓練データとテストデータの位置関係に着目
splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)


# 1 モデル構築 --------------------------------------------------------------------------------

# ＜ポイント＞
# - ParnsipとWorkflowsを用いたモデル構築プロセスを確認
# - 以下の３つの学習済モデルを作成しておく
#   --- 1-1 ARIMAモデル（Parsnip）
#   --- 1-2 ARIMAモデル（Workflow & フーリエ変換あり）
#   --- 1-3 ElasticNet(GLMNET) + XREGS（Workflow & フルレシピ）

# ＜参考＞
# General Interface for ARIMA Regression Models
# https://business-science.github.io/modeltime/reference/arima_reg.html


# * 1-0 準備 --------------------------------------------------------------

# データ確認
# --- 訓練データ
splits %>%
  training() %>%
  select(optins_trans, optin_time)

# プロット確認
splits %>%
  training() %>%
  plot_time_series(.date_var = optin_time,
                   .value    = optins_trans)


# * 1-1 ARIMAモデル（Parsnip） -----------------------------------------------

# ＜ポイント＞
# - Parsnipを用いてモデル定義
#   --- workflowのほうが柔軟性のあるモデル定義が可能(レシピとモデルの分割)


# モデル構築
# --- 学習済モデルの構築
# --- Parsnipの場合はRecipesで作成したデータをこの時点で与えなければならない
# --- 元データに含まれる説明変数は使っていない
model_fit_arima <-
  arima_reg() %>%
    set_engine("auto_arima") %>%
    fit(optins_trans ~ optin_time, data = training(splits))

# 確認
model_fit_arima %>% print()


# * 1-2 ARIMAモデル（Workflow & フーリエ変換あり） -------------------------------

# ＜ポイント＞
# - workflowを用いてモデル定義
#   --- レシピとモデルを独立して指定することが可能
#   --- モデルフォーミュラはレシピで記述する
#   --- モデル構築時点でレシピ適用後のデータセットにしておく必要がない


# モデル構築
# --- この時点では学習しない
model_spec_arima <-
  arima_reg() %>%
    set_engine("auto_arima")

# レシピ作成
# --- 元データに含まれる説明変数は使っていない
# --- フーリエ変換のレシピのみ追加
recipe_spec_fourier <-
  recipe(optins_trans ~ optin_time, data = training(splits)) %>%
    step_fourier(optin_time, period = c(7, 14, 30, 90), K = 1) 

# データ確認
# --- レシピ適用後
recipe_spec_fourier %>% prep() %>% juice() %>% glimpse()

# ワークフロー定義＆学習
# --- モデルとレシピを分割して設定
# --- 学習済モデルの作成（modeltimeテーブルに登録する際は学習済モデルのため）
workflow_fit_arima <-
  workflow() %>%
    add_recipe(recipe_spec_fourier) %>%
    add_model(model_spec_arima) %>%
    fit(data = training(splits))

# 確認
workflow_fit_arima %>% print()


# *1-3 ElasticNet(GLMNET) + XREGS（Workflow & フルレシピ）----------------------

# ＜ポイント＞
# - XREGS(External Regressor)を用いてモデル定義
# - GLMNETでモデル構築
# - モデル比較用


# レシピ確認
# --- 6章で作成したレシピ
# --- フォーミュラ確認(outcome ~ predictor)
recipe_spec_2_lag %>% print()
recipe_spec_2_lag$var_info

# データ確認
recipe_spec_2_lag %>% prep() %>% juice()
recipe_spec_2_lag %>% prep() %>% juice() %>% glimpse()

# モデル構築
# --- Elastic Net
model_spec_glmnet <-
  linear_reg(penalty = 0.1, mixture = 0.5) %>%
    set_engine("glmnet")

# ワークフロー定義
# --- 6章で作成したレシピをそのまま使用
# --- 学習済モデルの作成（modeltimeに導入する際は学習済モデルでないといけない）
workflow_fit_glmnet <-
  workflow() %>%
    add_recipe(recipe_spec_2_lag) %>%
    add_model(model_spec_glmnet) %>%
    fit(data = training(splits))

# 確認
workflow_fit_glmnet %>% print()


# 2 モデルテーブルの構築 ----------------------------------------------------------------

# ＜ポイント＞
# - 学習済モデルをテーブルに追加する
#   --- 複数モデルを同時に実行するための準備
#   --- レシピ/モデル/データを変更してモデル比較することが可能


# テーブル構築
# --- テーブルに学習済モデルを追加（tibbleにネストしてモデルを追加）
# --- .model_descは更新可能
model_tbl <-
  modeltime_table(model_fit_arima,
                  workflow_fit_arima,
                  workflow_fit_glmnet) %>%
    update_model_description(3, "GLMNET - Lag Recipe")

# 確認
model_tbl %>% print()


# 3 検証準備（Calibration） -----------------------------------------------------------

# ＜ポイント＞
# - CALIBRATION(検査)はテストデータを統一的に適用してモデル評価のデータを準備する
# - 学習済の各モデルに同一のテストデータを適用して予測値と残差を得る
#   --- モデル評価はまだ行っていない


# データ確認
splits %>% testing()

# 予測データの作成
# --- テストデータで作成
# --- tibbleの中に結果が作成される
calibration_tbl <-
  model_tbl %>%
    modeltime_calibrate(new_data = testing(splits))

# 確認（テーブル全体）
# --- データフレーム構造
calibration_tbl %>% print()

# 確認（モデル1）
# --- テスト期間の56レコードのデータが格納されている（モデル評価に必要なデータ）
# --- 各モデルを適用して予測値(.prediction)と残差(.residuals)を算出している
# --- 残差を算出するため正解値(.actual)も格納されてている
calibration_tbl %>% slice(1) %>% unnest(.calibration_data)


# 4 モデル精度の検証（Test Accuracy）-------------------------------------------------

# ＜ポイント＞
# - Calculates common accuracy measures
# - MAE, MAPE, MASE, SMAPE, RMSE, R-SQUARED


# モデル精度の確認
# --- default_forecast_accuracy_metric_set()に用いてメトリックが選択される
# --- モデル3が最も優れている
calibration_tbl %>% modeltime_accuracy()

# モデル精度をテーブル表示
# --- {reactable}を用いたテーブル
calibration_tbl %>%
  modeltime_accuracy(metric_set = default_forecast_accuracy_metric_set()) %>%
  table_modeltime_accuracy(.interactive = TRUE,
                           bordered = TRUE,
                           resizable = TRUE)

# メトリックを独自設定
metric_set(mae, rmse, iic)

# モデル精度をテーブル表示
calibration_tbl %>%
    modeltime_accuracy(metric_set = metric_set(mae, rmse, iic))

# ＜参考＞
# デフォルト指定のメトリック
# --- {yardstick}のメトリックを使っている
default_forecast_accuracy_metric_set()
default_forecast_accuracy_metric_set
?default_forecast_accuracy_metric_set


# 5 予測結果の確認（Test Forecast）---------------------------------------------------

# ＜ポイント＞
# - 学習済みモデルをテストデータに適用して予測精度を検証する（アウトオブサンプル検証）
# - Visualize the out-of-sample forecast

# データ期間の確認
# --- 予測に使用するデータの確認
# --- 全期間/訓練期間/テスト期間の確認（日付確認）
data_prepared_tbl %>% tk_index() %>% tk_get_timeseries_summary() %>% select(1:4)
splits %>% training() %>% tk_index() %>% tk_get_timeseries_summary() %>% select(1:4)
splits %>% testing() %>% tk_index() %>% tk_get_timeseries_summary() %>% select(1:4)

# 予測データの作成
# --- 全期間の時系列データセットが作成される
pred_tbl <-
  calibration_tbl %>%
      modeltime_forecast(new_data      = testing(splits),
                         actual_data   = data_prepared_tbl,
                         conf_interval = 0.80)

# レコード数の確認
# --- 777レコード（609 + 56 * 3モデル）
pred_tbl %>%
  group_by(.model_desc, .key) %>%
  tally()

# プロット作成
# --- 予測データの作成
# --- プロット
pred_tbl %>%
  plot_modeltime_forecast(.legend_max_width = 60,
                          .legend_show = FALSE,
                          .conf_interval_show = TRUE,
                          .conf_interval_alpha = 0.20,
                          .conf_interval_fill = "lightblue",
                          .title = "Email Subscriber Forecast")


# ヘルプ参照
?plot_modeltime_forecast


# 6 リフィッティング(Refitting) ----------------------------------------------------

# ＜ポイント＞
# - REFITでは新たなデータセットを用いて各モデルの学習とCalibrationを行う
#   --- モデルテーブルのままモデルのアップデートが可能
#   --- 新たなモデルでCalibrationが行われる
#   --- Calibrationに用いるデータは従来定義したものが用いられる（変更可能）
# - 最終予測のため、検証データを含めてモデルを再定義する
#   --- 直前までのデータを活用


# * Refit ----------------------------------------------------------------

# モデルの再計算
# --- 全期間のデータを使用（最終予測のため直前までデータを使用して再訓練）
# --- 以下のメッセージはAuto Arimaを再計算している証拠
# --- frequency = 7 observations per 1 week
refit_tbl <-
  calibration_tbl %>%
    modeltime_refit(data = data_prepared_tbl)


# ＜参考＞
# REFITが行っていることを確認

# データ準備
# --- 元のデータの前半/後半に分割
data_prepared_tbl_head <- data_prepared_tbl %>% head(300)
data_prepared_tbl_tail <- data_prepared_tbl %>% tail(300)

# 期間の確認
# --- headには元のモデルの検証期間が含まれていない
data_prepared_tbl_head %>% tk_index() %>% tk_get_timeseries_summary() %>% select(1:4)
data_prepared_tbl_tail %>% tk_index() %>% tk_get_timeseries_summary() %>% select(1:4)

# リフィット
# --- 新しいデータでテーブルに登録したモデルを再学習
# --- Calibrateデータも作成されている
refit_tbl_head <- calibration_tbl %>% modeltime_refit(data = data_prepared_tbl_head)
refit_tbl_tail <- calibration_tbl %>% modeltime_refit(data = data_prepared_tbl_tail)

# データ確認
# --- どちらもCalibrateデータの期間は同じ（modeltime_calibrate()で指定したテストデータを使用）
refit_tbl_head %>% slice(1) %>% unnest(.calibration_data)
refit_tbl_tail %>% slice(1) %>% unnest(.calibration_data)

# Calibrateの再定義
# --- テストデータのレコード数を20に変更
# --- 新たにテストデータを定義すると、｢.calibration_data｣の結果が変わる
refit_tbl_head %>% modeltime_calibrate(new_data = head(testing(splits), 20))
refit_tbl_head %>% modeltime_calibrate(new_data = head(testing(splits), 20))


# 7 最終予測の作成（Final Forecast） ---------------------------------------------------

# ＜ポイント＞
# - リフィットの時点でテスト期間を含めてモデルを再学習済
# - 学習済モデルに予測期間を適用して最終予測を取得する


# 最終モデルのデータ期間
# --- 学習期間
# --- 予測期間
data_prepared_tbl %>% tk_index() %>% tk_get_timeseries_summary() %>% select(1:4)
forecast_tbl %>% tk_index() %>% tk_get_timeseries_summary() %>% select(1:4)

# 予測データの作成
final_tbl <-
  refit_tbl %>%
    modeltime_forecast(# h = "8 weeks",
                       new_data = forecast_tbl,
                       actual_data = data_prepared_tbl,
                       conf_interval = 0.80)

# データ確認
final_tbl %>% print()
final_tbl %>% group_by(.model_desc, .key) %>% tally()

# プロット作成
final_tbl %>%
  plot_modeltime_forecast(.legend_max_width = 25,
                          .conf_interval_fill = "lightblue",
                          .interactive = TRUE)

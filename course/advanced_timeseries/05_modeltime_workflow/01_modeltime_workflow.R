# ****************************************************************************
# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: MODELTIME WORKFLOW FOR FORECASTING
# ****************************************************************************


# ゴール ----
# {modeltime}の基本ワークフローを理解する


# 目的 ----
# - Modeltimeのワークフローを理解する
# - モデル精度のメトリックを理解する
# - 予測ホライズンと信頼区間を理解する
# - 何故refitをするかを理解する


# ＜目次＞
# 0 準備
# 1 モデル構築
# 2 モデルテーブルの構築
# 3 予測（CALIBRATION）
# 4 モデル精度の検証（TEST ACCURACY）
# 5 予測結果の確認（TEST FORECAST）
# 6 リフィッティング(REFITTING)


# 0 準備 ------------------------------------------------------------------------------------

# * LIBRARIES ----------------------------------------------------------

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

# レシピ取得
recipe_spec_2_lag <- feature_engineering_artifacts_list$recipes$recipe_spec_2

# データ確認
# --- 1系列の時系列データ（optins_trans）
# --- 前処理で説明変数を生成している
data_prepared_tbl %>% glimpse()
forecast_tbl %>% glimpse()


# * データ分割 ----------------------------------------------------------

# 時系列データ分割
# --- 直近がテストデータとなるように分割
splits <-
  data_prepared_tbl %>%
    time_series_split(assess = "8 weeks", 
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
#   --- 1 ARIMAモデル（Parsnip）
#   --- 2 ARIMAモデル（Workflow & フーリエ変換あり）
#   --- 3 ElasticNet(GLMNET) + XREGS（Workflow & フルレシピ）


# * Parsnip Model (ARIMA)  ----------------------------------------------------

# ＜ポイント＞
# - Parsnipを用いてモデル定義
#   --- workflowのほうが柔軟性のあるモデル定義が可能(RecipeとModelの分割)


# データ確認
# --- 訓練データ
splits %>% training()

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


# * Workflow (ARIMA + Date Features) ----------------------------------------

# ＜ポイント＞
# - workflowを用いてモデル定義


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

# ワークフロー定義
# --- モデルとレシピを分割して設定
# --- 学習済モデルの作成（modeltimeに導入する際は学習済モデルのため）
workflow_fit_arima <-
  workflow() %>%
    add_recipe(recipe_spec_fourier) %>%
    add_model(model_spec_arima) %>%
    fit(training(splits))

# 確認
workflow_fit_arima %>% print()


# * Workflow (GLMNET + XREGS) ----------------------------------------

# ＜ポイント＞
# - GLMNETでモデル構築
# - モデル比較用


# レシピ確認
# --- 6章で作成したレシピ
recipe_spec_2_lag %>% print()

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
    fit(training(splits))

# 確認
workflow_fit_glmnet %>% print()


# 2 モデルテーブルの構築 ----------------------------------------------------------------

# ＜ポイント＞
# - 学習済モデルをテーブルに追加する
#   --- 複数モデルを同時に実行＆比較するための準備


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


# 3 予測（CALIBRATION） --------------------------------------------------------------------

# ＜ポイント＞
# - 学習済みモデルをテストデータに適用して予測精度を検証する（インサンプル検証）
#   --- 各学習済モデルでのテストデータを用いたモデル評価はまだ行っていない
# - CALIBRATION(検査)はテストデータにフォーカスしたテーブルが作成される
#   --- このステップでは全期間の出力は想定していない


# 予測データの作成
# --- テストデータで作成
# --- tibbleの中に結果が作成される
calibration_tbl <-
  model_tbl %>%
    modeltime_calibrate(new_data = testing(splits))

# 確認
# --- データフレーム構造
calibration_tbl %>% print()

# 確認
# --- モデル1
# --- テスト期間の8週間分のデータが格納されている
calibration_tbl %>% slice(1) %>% unnest(.calibration_data)


# 4 モデル精度の検証（TEST ACCURACY）-------------------------------------------------

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

# 参考： メトリックセットの確認
# --- {yardstick}のメトリックを使っている
default_forecast_accuracy_metric_set()
default_forecast_accuracy_metric_set
?default_forecast_accuracy_metric_set

# メトリックを独自設定
metric_set(mae, rmse, iic)

# モデル精度をテーブル表示
calibration_tbl %>%
    modeltime_accuracy(metric_set = metric_set(mae, rmse, iic))


# 5 予測結果の確認（TEST FORECAST）---------------------------------------------------

# ＜ポイント＞
# - 学習済みモデルをテストデータに適用して予測精度を検証する（アウトオブサンプル検証）
# - Visualize the out-of-sample forecast

# データ期間の確認
# --- 予測に使用するデータの確認
# --- ｢全期間｣と｢テスト期間｣のデータ
data_prepared_tbl %>% tk_index() %>% tk_get_timeseries_summary()
splits %>% testing() %>% tk_index() %>% tk_get_timeseries_summary()

# レコード数
data_prepared_tbl %>% nrow()
splits %>% testing() %>% nrow()

# 予測データの作成
# --- 全期間の時系列データセットが作成される
# --- 777レコード（609 + 56 * 3モデル）
calibration_tbl %>%
    modeltime_forecast(new_data      = testing(splits),
                       actual_data   = data_prepared_tbl,
                       conf_interval = 0.80)

# プロット作成
# --- 予測データの作成
# --- プロット
calibration_tbl %>%
    modeltime_forecast(new_data      = testing(splits),
                       actual_data   = data_prepared_tbl,
                       conf_interval = 0.80) %>%
    plot_modeltime_forecast(.legend_max_width = 60,
                            .legend_show = FALSE,
                            .conf_interval_show = TRUE,
                            .conf_interval_alpha = 0.20,
                            .conf_interval_fill = "lightblue",
                            .title = "Email Subscriber Forecast")

# 参考：データ構造
# --- 777レコード（609 + 56 * 3モデル）
calibration_tbl %>%
    modeltime_forecast(new_data      = testing(splits),
                       actual_data   = data_prepared_tbl,
                       conf_interval = 0.80) %>%
    group_by(.model_desc) %>%
    tally()


# ヘルプ参照
?plot_modeltime_forecast


# 6 リフィッティング(REFITTING) ----------------------------------------------------

# ＜ポイント＞
# - REFITでは新たなデータセットに対して｢学習｣を行い｢予測結果｣を出力する
#   --- Automated models (e.g. "auto arima")はパラメータを再計算する
#   --- Non-automated models (e.g. "arima")は学習済みモデルをそのまま使用する
#   --- CALIBRATEは予測結果を出力するのみ
# - REFITを用いると、モデルテーブルのままモデルのアップデートが可能
#   --- 訓練データ自体が入れ替わると、個別モデルは学習しなおす必要がある
#   --- 訓練データが同じだとREFITによって一部のプロセスを省略できる


# * Refit ----------------------------------------------------------------

# モデルの再計算
# --- 全期間のデータを使用
refit_tbl <-
  calibration_tbl %>%
    modeltime_refit(data = data_prepared_tbl)



# * Final Forecast --------------------------------------------------------

# ＜ポイント＞
# - 'new_data' vs 'h'
# - 'actual_data'
# - Preprocessing

# プロット作成
# --- 予測データの作成
# --- プロット
refit_tbl %>%
    modeltime_forecast(# h = "8 weeks",
                       new_data = forecast_tbl,
                       actual_data = data_prepared_tbl,
                       conf_interval = 0.80) %>%
    plot_modeltime_forecast(.legend_max_width = 25,
                            .conf_interval_fill = "lightblue",
                            .interactive = TRUE)


# ******************************************************************************
# Library     : modeltime
# Theme       : Getting Started with Modeltime
# Update Date : 2021/8/7
# URL         : https://business-science.github.io/modeltime/articles/getting-started-with-modeltime.html
# ******************************************************************************


# ＜概要＞
# - {modeltime}は複数モデルをテーブル化して学習ワークフローをサポートするフレームワーク
#   --- ワークフローの提供
#   --- 時系列系のparsnip形式のモデルの提供
#   --- プロット作成のサポート


# ＜ワークフロ＞
# 1 データを収集して分割(Split)
# 2 複数のモデルを作成＆学習（Modeling）
# 3 学習済モデルをモデルテーブルに追加（Model Table）
# 4 テストデータで検証データの作成（Calibrate）
# 5 予測精度の評価を実行（Accuracy）
# 6 将来データを予測（Forecast）
# 7 必要に応じて再学習（Refit）


# ＜目次＞
# 0 準備
# 1 時系列データ分割
# 2 モデル構築
# 3 モデルテーブルに登録
# 4 検証用データの作成
# 5 予測精度の検証
# 6 将来データの予測


# 0 準備 ---------------------------------------------------------------------------

# ライブラリ
library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)


# プロット設定
interactive <- FALSE

# データ準備
m750 <- m4_monthly %>% filter(id == "M750")

# データ確認
m750 %>% print()
m750 %>% plot_time_series(date, value, .interactive = interactive)


# 1 時系列データ分割 -----------------------------------------------------------------

# データ分割
splits <- m750 %>% initial_time_split(prop = 0.9)

# 確認
splits %>% print()


# 2 モデル構築 -----------------------------------------------------------------

# ＜ポイント＞
# - 単なるモデル定義ではなく、学習済モデルを作成する
# - {modeltime}や{parsnip}のモデルオブジェクト、{workflow}のオブジェクトを扱うことができる

# モデル1
# --- Auto ARIMA（Modeltime）
model_fit_arima_no_boost <-
  arima_reg() %>%
    set_engine(engine = "auto_arima") %>%
    fit(value ~ date, data = training(splits))

# モデル2
# --- Boosted Auto ARIMA（Modeltime）
model_fit_arima_boosted <-
  arima_boost(min_n = 2,
              learn_rate = 0.015) %>%
    set_engine(engine = "auto_arima_xgboost") %>%
    fit(value ~ date + as.numeric(date) + factor(month(date, label = TRUE), ordered = F),
        data = training(splits))

# モデル3
# --- Exponential Smoothing（Modeltime）
model_fit_ets <-
  exp_smoothing() %>%
    set_engine(engine = "ets") %>%
    fit(value ~ date, data = training(splits))

# モデル4
# --- Prophet（Modeltime）
model_fit_prophet <-
  prophet_reg() %>%
    set_engine(engine = "prophet") %>%
    fit(value ~ date, data = training(splits))

# モデル5
# --- Linear Regression (Parsnip)
model_fit_lm <-
  linear_reg() %>%
    set_engine("lm") %>%
    fit(value ~ as.numeric(date) + factor(month(date, label = TRUE), ordered = FALSE),
        data = training(splits))

# モデル6
# --- MARS (Workflow)
# Model 6: earth ----
model_spec_mars <-
  mars(mode = "regression") %>%
    set_engine("earth")

recipe_spec <-
  recipe(value ~ date, data = training(splits)) %>%
    step_date(date, features = "month", ordinal = FALSE) %>%
    step_mutate(date_num = as.numeric(date)) %>%
    step_normalize(date_num) %>%
    step_rm(date)

wflw_fit_mars <-
  workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec_mars) %>%
    fit(training(splits))


# 3 モデルテーブルに登録 -----------------------------------------------------------------

# テーブル登録
models_tbl <-
  modeltime_table(model_fit_arima_no_boost,
                  model_fit_arima_boosted,
                  model_fit_ets,
                  model_fit_prophet,
                  model_fit_lm,
                  wflw_fit_mars)

# 確認
models_tbl %>% print()


# 4 検証用データの作成 ------------------------------------------------------------------

# ＜ポイント＞
# キャリブレーションのデータは以下で使用される
# --- .residuals  ⇒ modeltime_forecast()で使用
# --- .prediction ⇒ modeltime_accuracy()で使用

# キャリブレーション
# --- 検証データの作成
calibration_tbl <-
  models_tbl %>%
    modeltime_calibrate(new_data = testing(splits))

# 確認
calibration_tbl %>% print()
calibration_tbl$.calibration_data[1]


# 5 予測精度の検証 ----------------------------------------------------------------------

# ＜デフォルトのメトリック＞
# MAE   - Mean absolute error, mae()
# MAPE  - Mean absolute percentage error, mape()
# MASE  - Mean absolute scaled error, mase()
# SMAPE - Symmetric mean absolute percentage error, smape()
# RMSE  - Root mean squared error, rmse()
# RSQ   - R-squared, rsq()

# 予測精度のテーブル出力
calibration_tbl %>%
    modeltime_accuracy() %>%
    table_modeltime_accuracy(.interactive = interactive)


# 6 将来データの予測 -----------------------------------------------------------------------

# リフィット
# --- 新しいデータで再度学習
refit_tbl <-
  calibration_tbl %>%
    modeltime_refit(data = m750)

# プロット
refit_tbl %>%
    modeltime_forecast(h = "3 years", actual_data = m750) %>%
    plot_modeltime_forecast(.legend_max_width = 25,
                            .interactive      = interactive)

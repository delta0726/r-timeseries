# ***************************************************************************************
# Library     : modeltime.resample
# Theme       : Resampling Panel Data
# Update Date : 2021/8/7
# URL         : https://business-science.github.io/modeltime.resample/articles/panel-data.html
# ******************************************************************************


# ＜概要＞
# - パネルデータにおける時系列クロスバリデーションのフローを確認



# ＜目次＞
# 0 準備
# 1 時系列データ分割
# 2 データ準備
# 3 リサンプリングデータの作成
# 4 特徴量エンジニアリング
# 5 モデル構築
# 6 モデルテーブルの定義
# 7 単一リサンプルセットで予測作成
# 8 全てのリサンプルデータで学習


# 0 準備 ---------------------------------------------------------------------------

# ライブラリ
library(tidymodels)
library(modeltime)
library(modeltime.resample)
library(timetk)
library(tidyverse)
library(tidyquant)


# データ確認
# --- 7系列を含むパネルデータ
walmart_sales_weekly %>% print()
walmart_sales_weekly %>% group_by(id) %>% tally()

# プロット作成
walmart_sales_weekly %>%
  group_by(id) %>%
  plot_time_series(Date, Weekly_Sales, .facet_ncol = 3, .interactive = FALSE)


# 1 時系列データ分割 -----------------------------------------------------------------

# データ分割
splits <- m750 %>% initial_time_split(prop = 0.9)

# 確認
splits %>% print()

# 期間確認
# --- 時系列方向に分割されている
splits %>% training() %>% tk_summary_diagnostics() %>% select(1:4)
splits %>% testing() %>% tk_summary_diagnostics() %>% select(1:4)


# 2 データ準備 ----------------------------------------------------------------------

# データ加工
# --- 列の選択
# --- 将来データの作成
# --- idのファクター変換（パネルキー）
full_data_tbl <-
  walmart_sales_weekly %>%
    select(id, Date, Weekly_Sales) %>%
    group_by(id) %>%
    future_frame(.date_var   = Date,
                 .length_out = "3 months",
                 .bind_data  = TRUE) %>%
    ungroup() %>%
    mutate(id = fct_drop(id))

# データ確認
# --- 学習用データ（Train/Testに分割して使用）
# --- 予測用データ（future_frame()で作成したもの）
data_prepared_tbl <- full_data_tbl %>% filter(!is.na(Weekly_Sales))
future_tbl        <- full_data_tbl %>% filter(is.na(Weekly_Sales))


# 3 リサンプリングデータの作成 ----------------------------------------------------------

# データのリサンプリング
# --- Cumulative=TRUEなので、Expandでデータが作成される
walmart_tscv <-
  data_prepared_tbl %>%
    time_series_cv(date_var    = Date,
                   assess      = "3 months",
                   skip        = "3 months",
                   cumulative  = TRUE,
                   slice_limit = 6)

# データ確認
walmart_tscv %>% print()

# プロット確認
walmart_tscv %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(Date, Weekly_Sales,
                           .facet_ncol = 2, .interactive = F)

# 参考：プロット確認
# --- Cumulative=FALSE
data_prepared_tbl %>%
  time_series_cv(date_var    = Date,
                 assess      = "3 months",
                 skip        = "3 months",
                 cumulative  = FALSE,
                 slice_limit = 6) %>% 
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(Date, Weekly_Sales,
                           .facet_ncol = 1, .interactive = F)


# 4 特徴量エンジニアリング ---------------------------------------------------------------

# レシピ定義
recipe_spec <-
  recipe(Weekly_Sales ~ ., data = training(walmart_tscv$splits[[1]])) %>%
    step_timeseries_signature(Date) %>%
    step_rm(matches("(.iso$)|(.xts$)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>%
    step_mutate(Date_week = factor(Date_week, ordered = TRUE)) %>%
    step_dummy(all_nominal(), one_hot = TRUE)

# データ確認
recipe_spec %>% prep() %>% juice()


# 5 モデル構築 --------------------------------------------------------------------------

# ＜ポイント＞
# - モデルテーブルに登録するための学習済モデルを定義する
# - モデルテーブルは{parsnip}と{workflow}の両方を受け取ることができる


# モデル定義
# --- prophet
# --- xgboost
# --- prophet_xgboost
model_prophet <- prophet_reg() %>% set_engine("prophet")
model_xgboost <- boost_tree() %>% set_engine("xgboost")
model_prophet_boost <-
  prophet_boost(seasonality_daily  = FALSE,
                seasonality_weekly = FALSE,
                seasonality_yearly = FALSE) %>%
    set_engine("prophet_xgboost")

# ワークフロー
# --- prophet
wflw_fit_prophet <-
  workflow() %>%
    add_model(model_prophet) %>%
    add_recipe(recipe_spec) %>%
    fit(training(walmart_tscv$splits[[1]]))

# ワークフロー
# --- xgboost
wflw_fit_xgboost <-
  workflow() %>%
    add_model(model_xgboost) %>%
    add_recipe(recipe_spec %>% step_rm(Date)) %>%
    fit(training(walmart_tscv$splits[[1]]))

# ワークフロー
# --- prophet_xgboost
wflw_fit_prophet_boost <-
  workflow() %>%
    add_model(model_prophet_boost) %>%
    add_recipe(recipe_spec) %>%
    fit(training(walmart_tscv$splits[[1]]))


# 6 モデルテーブルの定義 --------------------------------------------------------------

# テーブル登録
model_tbl <- 
  modeltime_table(wflw_fit_prophet, 
                  wflw_fit_xgboost, 
                  wflw_fit_prophet_boost)

# 確認
model_tbl %>% print()



# 7 単一リサンプルセットで予測作成 -----------------------------------------------------

# 検証データの作成
df_test <- walmart_tscv$splits[[1]] %>% testing()
df_test %>% print()

# 検証用データの作成
# --- キャリブレーション
calibration_tbl <- 
  model_tbl %>%
    modeltime_calibrate(new_data = df_test)

# 予測データの作成
# --- caliblationから予測データを作成すると信頼区間が得られる
forecast_panel_tbl <- 
  calibration_tbl %>%
    modeltime_forecast(new_data      = df_test, 
                       actual_data   = data_prepared_tbl,
                       keep_data = TRUE) 

# プロット作成
# --- グループ別
forecast_panel_tbl %>%
  group_by(id) %>%
  plot_modeltime_forecast(.facet_ncol  = 3, 
                          .y_intercept = 0,  
                          .interactive = FALSE, 
                          .title       = "Panel Forecasting | 7 Time Series Groups")


# 8 全てのリサンプルデータで学習 -------------------------------------------------------

# クロスバリデーション学習
# --- モデルテーブルの登録モデル全てでクロスバリデーションを実施
resample_results <- 
  model_tbl %>%
    modeltime_fit_resamples(resamples = walmart_tscv, 
                            control   = control_resamples(verbose = FALSE))

# データ確認
resample_results %>% print()


# 予測精度の評価
resample_results %>%
  modeltime_resample_accuracy(summary_fns = list(mean = mean, sd = sd)) %>%
  table_modeltime_accuracy(.interactive = FALSE)

# 予測精度の可視化
resample_results %>%
  plot_modeltime_resamples(.summary_fn  = mean, 
                           .point_size  = 3,
                           .interactive = FALSE)

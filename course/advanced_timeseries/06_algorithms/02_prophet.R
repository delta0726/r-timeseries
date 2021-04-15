# *****************************************************************************
# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: PROPHET
# *****************************************************************************


# ゴール ----
# - Prophetモデルを理解する

# OBJECTIVES ----
# - Describe PROPHET using Prophet Components
# - モデルのパラメータを確認する
# - ModeltimeでPROPHETを実行する


# ＜PROPHETモデル＞
# - 周期性を持つデータの予測を想定したアルゴリズム
# - ARIMAモデルよりも使いやすいアルゴリズム
#   --- 季節性の分析が簡単（ダミー変数などの設定なし、ドメイン知識のみ必要）
#   --- パラメータチューニングが簡単（統計知識が必要ない）
#   --- 初期設定でもある程度の予測精度がある


# ＜参考＞
# - Prophet入門【R編】Facebookの時系列予測ツール
# - https://www.slideshare.net/hoxo_m/prophetrfacebook


# ＜目次＞
# 0 準備
# 1 PROPHETモデル
# 2 PROPHETのコンセプト
# 3 XREGSモデル
# 4 モデル保存


# 0 準備 ------------------------------------------------------------------------

# LIBRARIES & SETUP ----

# Time Series ML
library(tidymodels)
library(modeltime)
library(prophet)

# Core
library(tidyverse)
library(lubridate)
library(timetk)
library(plotly)

# 関数
source("00_scripts/01_calibrate_and_plot.R")


# * データ準備 ---------------------------------------------------------

# データロード
artifacts_list <- read_rds("00_models/feature_engineering_artifacts_list.rds")
data_prepared_tbl <- artifacts_list$data$data_prepared_tbl

# データ確認
data_prepared_tbl %>% print()
data_prepared_tbl %>% glimpse()
data_prepared_tbl %>% tk_index() %>% tk_get_timeseries_summary()


# * データ分割 ---------------------------------------------------------

# 時系列データ分割
splits <-
  data_prepared_tbl %>%
    time_series_split(assess = "8 weeks", cumulative = TRUE)

# プロット作成
# --- 訓練期間とテスト期間の確認
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)


# 1 PROPHETモデル -----------------------------------------------------------------

# * Modeltime Model ---------------------------------------------

# ヘルプ参照
?prophet_reg

# モデル構築
# --- 学習済モデルの構築
# --- changepoint： Piecewise Regression
# --- seasonality_yearly： フーリエ
# --- seasonality_weekly： ダミー変数
model_fit_prophet <-
  prophet_reg(changepoint_num    = 25,
              changepoint_range  = 0.8,
              seasonality_yearly = TRUE,
              seasonality_weekly = TRUE) %>%
    set_engine("prophet") %>%
    fit(optins_trans ~ optin_time, data = training(splits))

# プロット作成（予測データ）
# --- モデルテーブル登録
# --- モデル精度の検証
# --- 予測データの作成
modeltime_table(model_fit_prophet) %>%
  modeltime_calibrate(new_data = testing(splits)) %>%
  modeltime_forecast(new_data = testing(splits),
                     actual_data = data_prepared_tbl) %>%
  plot_modeltime_forecast()


# 2 PROPHETのコンセプト -------------------------------------------------------------

# * Extract model ------------------------------------------------

# モデル抽出
prophet_model <- model_fit_prophet$fit$models$model_1

# 確認
prophet_model %>% print()
prophet_model %>% names()

# 予測データの作成
# --- 全期間
prophet_fcst <-
  prophet_model %>%
    predict(newdata = training(splits) %>% rename(ds = 1, y = 2))


# * モデルの可視化 ----------------------------------------------------

# プロット作成
# --- prophet::plot()
# --- prophet::add_changepoints_to_plot()
g <-
  prophet_model %>%
    plot(prophet_fcst) +
    add_changepoints_to_plot(prophet_model)

# プロット表示
# --- Actural： ドットチャート
# --- Predict： ラインチャート
# --- Vertical： モデル変更点
ggplotly(g)


# * Visualize Additive Components ------------------------------------

prophet_model %>% prophet_plot_components(prophet_fcst)


# 3 XREGSモデル -----------------------------------------------------------------------

# * Model ------------------------------------------------------------

# モデル構築
# --- lab_eventを説明変数に追加
model_fit_prophet_xregs <-
  prophet_reg(seasonality_yearly = TRUE) %>%
    set_engine("prophet") %>%
    fit(optins_trans ~ optin_time + lab_event, data = training(splits))


# * Calibration（検査） -----------------------------------------------

# モデル検証
# --- モデルテーブルに登録
# --- 検証データで予測精度の検証用データを作成
calibration_tbl <-
  modeltime_table(model_fit_prophet,
                  model_fit_prophet_xregs) %>%
    modeltime_calibrate(testing(splits))


# * 予測結果の確認 ----------------------------------------------------

# プロット作成（予測データ）
# --- 検証データ=予測データ
# --- プロットに信頼区間の追加
calibration_tbl %>%
  modeltime_forecast(new_data = testing(splits),
                     actual_data = data_prepared_tbl) %>%
  plot_modeltime_forecast(.conf_interval_show = FALSE)


# * モデル精度の検証 ----------------------------------------------------

# メトリックの出力
calibration_tbl %>% modeltime_accuracy()


# * リフィット ------------------------------------------------------------

# プロット作成
# --- 全期間データでリフィット
calibration_tbl %>%
  modeltime_refit(data = data_prepared_tbl) %>%
  modeltime_forecast(new_data = artifacts_list$data$forecast_tbl,
                     actual_data = data_prepared_tbl) %>%
  plot_modeltime_forecast()


# 4 モデル保存 ---------------------------------------------------------------

# モデル抽出
#model_fit_best_prophet <-
#  calibration_tbl %>%
#    slice(2) %>%
#    pluck(".model", 1)

# モデル保存
# model_fit_best_prophet %>% write_rds( "00_models/model_fit_best_prophet.rds")

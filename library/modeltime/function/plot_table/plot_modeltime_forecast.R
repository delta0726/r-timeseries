# ***************************************************************************************
# Library   : modeltime
# Function  : plot_modeltime_forecast
# Created on: 2021/8/5
# URL       : https://business-science.github.io/modeltime/reference/plot_modeltime_forecast.html
# ***************************************************************************************


# ＜概要＞
# - timetk::plot_time_series()のラッパー関数
#   --- 信頼区間の表示が可能


# ＜構文＞
# plot_modeltime_forecast(
#   .data,
#   .conf_interval_show = TRUE,
#   .conf_interval_fill = "grey20",
#   .conf_interval_alpha = 0.2,
#   .smooth = FALSE,
#   .legend_show = TRUE,
#   .legend_max_width = 40,
#   .title = "Forecast Plot",
#   .x_lab = "",
#   .y_lab = "",
#   .color_lab = "Legend",
#   .interactive = TRUE,
#   .plotly_slider = FALSE,
#   ...
# )


# ＜使用例＞
# 0 準備
# 1 データ分割
# 2 学習
# 3 プロット作成


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(lubridate)
library(timetk)
library(parsnip)
library(rsample)
library(modeltime)


# データ準備
# --- 1系列のみ抽出
m750 <- m4_monthly %>% filter(id == "M750")

# データ確認
m750 %>% print()


# 1 データ分割 ------------------------------------------------------------------------------

# データ分割
splits <- m750 %>% initial_time_split(prop = 0.9)

# データ確認
splits %>% training() %>% tk_summary_diagnostics() %>% select(1:4)
splits %>% testing() %>% tk_summary_diagnostics() %>% select(1:4)


# 2 学習 ------------------------------------------------------------------------------------

# 学習済モデルの構築
# --- モデル定義＆学習
model_fit_arima <-
  arima_reg() %>%
    set_engine(engine = "auto_arima") %>%
    fit(value ~ date, data = training(splits))

# モデルテーブルに登録
models_tbl <-
  modeltime_table(model_fit_arima)

# 予測データの作成
# --- キャリブレートを実行すると信頼区間が得られる
df_forecast <-
  models_tbl %>%
    modeltime_calibrate(new_data = testing(splits)) %>%
    modeltime_forecast(new_data    = testing(splits),
                       actual_data = m750)

# データ確認
# --- 予測期間には信頼区間の情報が含まれる
# --- actualは全期間、predictionはテスト期間
df_forecast %>% group_split(.model_desc)
df_forecast %>% group_split(.model_desc) %>% map(tk_summary_diagnostics) %>% map(select, 1:4)


# 3 プロット作成 -----------------------------------------------------------------------------

# プロット作成
df_forecast %>%
  plot_modeltime_forecast(.interactive = FALSE)

# ***************************************************************************************
# Library   : modeltime
# Function  : plot_modeltime_residuals
# Created on: 2021/8/5
# URL       : https://business-science.github.io/modeltime/reference/plot_modeltime_residuals.html
# ***************************************************************************************


# ＜概要＞
# - 残差データを用いて｢時系列推移｣｢ACF｣｢Seaconality｣のプロットを作成する
#   --- Time       : plot_time_series()
#   --- ACF        : plot_acf_diagnostics()
#   --- Seasonality: plot_seasonal_diagnostics()


# ＜構文＞
# plot_modeltime_residuals(
#   .data,
#   .type = c("timeplot", "acf", "seasonality"),
#   .smooth = FALSE,
#   .legend_show = TRUE,
#   .legend_max_width = 40,
#   .title = "Residuals Plot",
#   .x_lab = "",
#   .y_lab = "",
#   .color_lab = "Legend",
#   .interactive = TRUE,
#   ...
# )


# ＜使用例＞
# 0 準備
# 1 データ分割
# 2 学習
# 3 残差データの取得
# 4 残差プロットの作成


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(lubridate)
library(timetk)
library(parsnip)
library(rsample)


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


# 3 残差データの取得 ----------------------------------------------------------------------------

# 残差データの取得
# --- キャリブレーションで残差を取得
# --- 1つのデータフレームに集約
residuals_tbl <-
  models_tbl %>%
    modeltime_calibrate(new_data = testing(splits)) %>%
    modeltime_residuals()


# 4 残差プロットの作成 --------------------------------------------------------------------------

# プロット作成
# --- 時系列プロット
residuals_tbl %>%
    plot_modeltime_residuals(.type = "timeplot",
                             .interactive = FALSE)

# プロット作成
# --- ACFプロット
residuals_tbl %>%
    plot_modeltime_residuals(.type = "acf",
                             .interactive = FALSE)

# プロット作成
# --- 季節性プロット
residuals_tbl %>%
    plot_modeltime_residuals(.type = "seasonality",
                             .interactive = FALSE)
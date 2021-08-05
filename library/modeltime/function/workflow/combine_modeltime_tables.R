# ***************************************************************************************
# Library   : modeltime
# Function  : combine_modeltime_tables
# Created on: 2021/8/6
# URL       : https://business-science.github.io/modeltime/reference/combine_modeltime_tables.html
# ***************************************************************************************


# ＜概要＞
# - 複数のモデルテーブルを1つに結合する


# ＜構文＞
# combine_modeltime_tables(...)


# ＜使用例＞
# 0 準備
# 1 モデル構築
# 2 モデルテーブルの結合


# 0 準備 ---------------------------------------------------------------------------------------

# ライブラリ
library(modeltime)
library(tidymodels)
library(tidyverse)
library(timetk)
library(lubridate)


# データ準備
m750 <- m4_monthly %>% filter(id == "M750")

# データ分割
splits <- m750 %>% time_series_split(assess = "3 years", cumulative = TRUE)


# 1 モデル構築 ---------------------------------------------------------------------------------

# モデル定義＆学習
# --- Auto Arima
model_fit_arima <-
  arima_reg() %>%
    set_engine("auto_arima") %>%
    fit(value ~ date, training(splits))

# モデル定義＆学習
# --- Prophet
model_fit_prophet <-
  prophet_reg() %>%
    set_engine("prophet") %>%
    fit(value ~ date, training(splits))


# 2 モデルテーブルの結合 --------------------------------------------------------------------------

# モデルテーブルに登録
# --- 個別に登録
model_tbl_1 <- modeltime_table(model_fit_arima)
model_tbl_2 <- modeltime_table(model_fit_prophet)

# モデルテーブルの結合
combine_modeltime_tables(model_tbl_1, model_tbl_2)

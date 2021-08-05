# ***************************************************************************************
# Library   : modeltime
# Function  : recursive
# Created on: 2021/8/6
# URL       : https://business-science.github.io/modeltime/reference/recursive.html
# ***************************************************************************************


# ＜概要＞
# - パースニップまたはワークフロー回帰モデルから再帰的時系列モデルを作成する


# ＜構文＞
# recursive(object, transform, train_tail, id = NULL, ...)


# ＜再帰モデル＞
# - 予測を使用して、独立した特徴量(ラグ系列)の新しい値を生成します


# ＜使用例＞
# **** 単独データ ****
# 0 準備
# 1 将来データの作成
# 2 モデル用のデータ作成
# 3 モデル構築
# 4 予測

# **** パネルデータ ****
# 10 準備
# 11 将来データの作成
# 12 モデル用のデータ作成
# 13 モデル構築
# 14 予測


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(modeltime)
library(tidymodels)
library(tidyverse)
library(lubridate)
library(timetk)
library(slider)


# データ確認
m750 %>% print()
m750 %>% group_by(id) %>% tally()
m750 %>% tk_summary_diagnostics() %>% select(1:4)


# 1 将来データの作成 ------------------------------------------------------------------------

# ホライズンの指定
FORECAST_HORIZON <- 24

# 将来データを追加
m750_extended <-
  m750 %>%
    group_by(id) %>%
    future_frame(.date_var   = date,
                 .length_out = FORECAST_HORIZON,
                 .bind_data  = TRUE) %>%
    ungroup()

# データ確認
m750_extended %>% tk_summary_diagnostics() %>% select(1:4)


# 2 モデル用のデータ作成 ---------------------------------------------------------------------

# 関数定義
# --- ラグ系列の追加
# --- ローリング平均系列の追加
lag_roll_transformer <- function(data){
    data %>%
      tk_augment_lags(value, .lags = 1:12) %>%
      mutate(rolling_mean_12 = lag(slide_dbl(
            value, .f = mean, .before = 12, .complete = FALSE
        ), 1))
}

# データ準備
# --- 系列追加 
m750_rolling <- 
  m750_extended %>%
    lag_roll_transformer() %>%
    select(-id)

# 訓練データ
train_data <- m750_rolling %>%drop_na()

# 予測用データ
# --- feature_frame()で作成した系列
future_data <- m750_rolling %>%filter(is.na(value))



# 3 モデル構築 ---------------------------------------------------------------------------------

# 単純な時系列線形回帰モデル
# --- Use only date feature as regressor
model_fit_lm <- 
  linear_reg() %>%
    set_engine("lm") %>%
    fit(value ~ date, data = train_data)

# 再帰的な時系列線形回帰モデル
# --- Autoregressive Forecast
# --- Use date plus all lagged features
# --- Add recursive() w/ transformer and train_tail
model_fit_lm_recursive <- 
  linear_reg() %>%
    set_engine("lm") %>%
    fit(value ~ ., data = train_data) %>%
    recursive(transform  = lag_roll_transformer, 
              train_tail = tail(train_data, FORECAST_HORIZON))


# 確認
model_fit_lm
model_fit_lm_recursive


# 4 予測 ---------------------------------------------------------------------------------------

# プロット作成
modeltime_table(model_fit_lm, 
                model_fit_lm_recursive) %>%
  update_model_description(2, "LM - Lag Roll") %>%
  modeltime_forecast(new_data    = future_data, 
                     actual_data = m750) %>%
  plot_modeltime_forecast(.interactive        = FALSE, 
                          .conf_interval_show = FALSE)



# 10 準備 ----------------------------------------------------------------------------------------

# データ確認
m4_monthly %>% print()
m4_monthly %>% group_by(id) %>% tally()


# 11 将来データの作成 ----------------------------------------------------------------------------

# 予測ホライズン
FORECAST_HORIZON <- 24

# 将来データを追加
m4_extended <- 
  m4_monthly %>%
    group_by(id) %>%
    future_frame(.length_out = FORECAST_HORIZON, 
                 .bind_data  = TRUE) %>%
    ungroup()


# 12 モデル用のデータ作成 ------------------------------------------------------------------------

# 関数定義
# --- ラグ系列の追加
# --- ローリング平均系列の追加
lag_transformer_grouped <- function(data){
  data %>%
    group_by(id) %>%
    tk_augment_lags(value, .lags = 1:FORECAST_HORIZON) %>%
    ungroup()
}

# データ準備
# --- 系列追加 
m4_lags <- 
  m4_extended %>%
  lag_transformer_grouped()

# 訓練データ
train_data <- m4_lags %>% drop_na()

# 予測用データ
# --- feature_frame()で作成した系列
future_data <- m4_lags %>% filter(is.na(value))


# 13 モデル構築 --------------------------------------------------------------------------------

# 再帰的パネル時系列回帰
# --- Modeling Autoregressive Panel Data
# --- recursive()のidでパネルキーを指定する
model_fit_lm_recursive <- 
  linear_reg() %>%
    set_engine("lm") %>%
    fit(value ~ ., data = train_data) %>%
    recursive(id         = "id", 
              transform  = lag_transformer_grouped,
              train_tail = panel_tail(train_data, id, FORECAST_HORIZON))


# 14 予測 ------------------------------------------------------------------------------------

modeltime_table(model_fit_lm_recursive) %>%
  modeltime_forecast(new_data    = future_data, 
                     actual_data = m4_monthly, 
                     keep_data   = TRUE) %>%
  group_by(id) %>%
  plot_modeltime_forecast(.interactive = FALSE, 
                          .conf_interval_show = FALSE)


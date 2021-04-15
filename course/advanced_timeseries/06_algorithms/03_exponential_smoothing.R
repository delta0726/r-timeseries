# *****************************************************************************
# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: EXPONENTIAL SMOOTHING
# *****************************************************************************


# ゴール ----
# - 指数平滑モデル(ETS)と季節性モデル(TBATS)と融合モデル()を理解する

# OBJECTIVES ----
# - ETS - Exponential Smoothing
# - TBATS - Multiple Seasonality Models
# - Seasonal Decomposition - Multiple Seasonality Models


# ＜目次＞
# 0 準備
# 1 ETSモデル (時系列指数平滑)
# 2 TBATSモデル
# 3 SEASONAL DECOMPOSITION
# 4 STLM ARIMA Model
# 5 複数モデルの評価


# 0 準備 ---------------------------------------------------------------------------

# * LIBRARIES & SETUP ------------------------------------------------

# Time Series ML
library(tidymodels)
library(modeltime)
library(forecast)

# Core
library(tidyverse)
library(lubridate)
library(timetk)


# * データ準備 ---------------------------------------------------------

# データロード
artifacts_list <- read_rds("00_models/feature_engineering_artifacts_list.rds")

# データ取得
data_prepared_tbl <- artifacts_list$data$data_prepared_tbl

# データ確認
data_prepared_tbl %>% print()
data_prepared_tbl %>% glimpse()
data_prepared_tbl %>% tk_index() %>% tk_get_timeseries_summary()


# * データ分割 --------------------------------------------------------

# 時系列データ分割
splits <-
  data_prepared_tbl %>%
    time_series_split(assess = "8 weeks", cumulative = TRUE)

# プロット作成
# --- 訓練期間とテスト期間の確認
splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(optin_time, optins_trans)

# 訓練データの加工
train_tbl <-
  splits %>%
    training() %>%
    select(optin_time, optins_trans)


# 1 EXPONENTIAL SMOOTHING (ETS) ----------------------------------------------------

# ＜ポイント＞
# - Error, Trend, Seasonal Model - Holt-Winters Seasonal
# - Automatic forecasting method based on Exponential Smoothing
# - Single Seasonality
# - 外部変数は使えない (完全に1変量のみ)


# * 指数平滑とは ----------------------------------------------------

?ets
?HoltWinters

# 関数定義
# --- 指数平滑
exp_smoother <- function(i, a) a * (1 - a) ^ i

# 変数定義
# --- 平滑係数
a <- 0.40

# 関数実行
exp_smoother(i = 2, a = a)

# プロット作成
# --- 0-10の数値を指数平滑変換
0:10 %>% exp_smoother(a) %>% plot()

# 計算イメージ
# --- 指数平滑平均
# --- 直近1期間の計算イメージ（15期間 / 平滑係数は0.4）
train_tbl %>%
  slice_tail(n = 15) %>%
  arrange(desc(optin_time)) %>%
  mutate(id = 0:(n()-1)) %>%
  mutate(wt = exp_smoother(id, a)) %>%
  mutate(value = wt * optins_trans) %>%
  summarise(value = sum(value))


# * ETS Model ------------------------------------------------------------

# ヘルプ参照
?exp_smoothing()

# モデル構築
# --- 学習済み
model_fit_ets <-
  exp_smoothing(error = "additive",
                trend = "additive",
                season = "additive") %>%
    set_engine("ets") %>%
    fit(optins_trans ~ optin_time, data = training(splits))

# プロット作成（予測データ）
# --- モデルテーブル登録
# --- モデル精度の検証
# --- 予測データの作成
modeltime_table(model_fit_ets) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_forecast(new_data = testing(splits),
                     actual_data = data_prepared_tbl) %>%
  plot_modeltime_forecast()


# 2 TBATSモデル --------------------------------------------------------------------

# ＜ポイント＞
# - 週次/月次の季節性など複数の季節性を扱うことができるモデル
#   --- ETSモデルの季節性を高度化したもの
#   --- 外部変数は使えない (完全に1変量のみ)


# ヘルプ参照
?tbats
?seasonal_reg()

# モデル構築
# --- 学習済み
model_fit_tbats <-
  seasonal_reg(seasonal_period_1 = 7,
               seasonal_period_2 = 30,
               seasonal_period_3 = 365) %>%
    set_engine("tbats") %>%
    fit(optins_trans ~ optin_time, training(splits))

# プロット作成（予測データ）
# --- モデルテーブル登録（ETS, TBATS）
# --- モデル精度の検証
# --- 予測データの作成
modeltime_table(model_fit_ets,
                model_fit_tbats) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_forecast(new_data = testing(splits),
                     actual_data = data_prepared_tbl) %>%
  plot_modeltime_forecast()


# 3 SEASONAL DECOMPOSITION ------------------------------------------------------

# ＜ポイント＞
# - Uses seasonal decomposition to model 
#   trend & seasonality separately
#   - Trend modeled with ARIMA or ETS
#   - Seasonality modeled with Seasonal Naive (SNAIVE)
# - Can handle multiple seasonality
# - ARIMA version accepts XREGS, ETS does not


# * Multiple Seasonal Decomposition --------------------------------------------

# プロット作成
train_tbl$optins_trans %>%
  msts(seasonal.periods = c(7, 30, 364/2)) %>%
  mstl() %>%
  autoplot()


# * STLM ETS Model ------------------------------------------------------------

# モデル構築
# --- 学習済み
model_fit_stlm_ets <-
  seasonal_reg(seasonal_period_1 = 7,
               seasonal_period_2 = 30,
               seasonal_period_3 = 364 / 2) %>%
    set_engine("stlm_ets") %>%
    fit(optins_trans ~ optin_time, data = training(splits))

model_fit_stlm_ets$fit$models$model_1$stl %>% autoplot()


# 4 STLM ARIMA Model ------------------------------------------------------

# * 外部変数なし ------------------------------------------------

# モデル構築
# --- 学習済み
model_fit_stlm_arima <-
  seasonal_reg(seasonal_period_1 = 7,
               seasonal_period_2 = 30,
               seasonal_period_3 = 364 / 2) %>%
    set_engine("stlm_arima") %>%
    fit(optins_trans ~ optin_time, data = training(splits))

# 確認
model_fit_stlm_arima


# * 外部変数あり ------------------------------------------------

# モデル構築
# --- 学習済み
model_fit_stlm_arima_xregs <-
  seasonal_reg(seasonal_period_1 = 7,
               seasonal_period_2 = 30,
               seasonal_period_3 = 364 / 2) %>%
    set_engine("stlm_arima") %>%
    fit(optins_trans ~ optin_time + lab_event, data = training(splits))

# 確認
model_fit_stlm_arima_xregs


# 5 複数モデルの評価 ------------------------------------------------------

# モデルテーブルに登録
model_tbl <-
  modeltime_table(model_fit_ets,
                  model_fit_tbats,
                  model_fit_stlm_ets,
                  model_fit_stlm_arima,
                  model_fit_stlm_arima_xregs)

# 検査準備
calibration_tbl <-
  model_tbl %>%
    modeltime_calibrate(testing(splits))

# プロット作成
# --- 予測データの確認
calibration_tbl %>%
  modeltime_forecast(new_data = testing(splits),
                     actual_data = data_prepared_tbl) %>%
  plot_modeltime_forecast(.conf_interval_fill = "lightblue",
                          .conf_interval_alpha = 0.1)


# モデル精度の確認
calibration_tbl %>% modeltime_accuracy()

# リフィット
refit_tbl <-
  calibration_tbl %>%
    modeltime_refit(data = data_prepared_tbl)

refit_tbl %>%
    modeltime_forecast(new_data = artifacts_list$data$forecast_tbl,
                       actual_data = data_prepared_tbl) %>%
    plot_modeltime_forecast(.conf_interval_show = FALSE)


# 6 モデル保存 ------------------------------------------------------

# モデル保存
#calibration_tbl %>%
#  write_rds("00_models/calibration_tbl_ets_tbats.rds")

# モデル確認
#read_rds("00_models/calibration_tbl_ets_tbats.rds")

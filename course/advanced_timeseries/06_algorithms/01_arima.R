# ****************************************************************************
# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: ARIMA MODELING
# ****************************************************************************


# GOAL:
# ARIMAモデルを理解する

# OBJECTIVES ----
# - 線形回帰モデルを使ってARIMAモデルの考え方を理解する
#   --- SARIMAモデルへの拡張（季節自己回帰和分移動平均モデル）
#   --- SARIMAXモデルへの拡張（季節自己回帰和分移動平均モデル + 外部変数）
# - ModeltimeでARIMAを作成
# - ModeltimeでAuto ARIMAを作成


# ＜ARIMAモデル＞
# 概要:
# - シンプルな線形回帰の概念に基づく予測モデル
# - 非定常過程に対する時系列モデル
#   --- ラグや差分を取ることで定常過程に変換している
#   --- ARモデル/ARMAモデルなどはARIMAモデルを理解するための準備

# メリット:
# - 差分系列作成やラグ処理を自動的に行って予測する（フォーミュラで表現する必要なし）
# - ハイパーパラメータを自動的にチューニングする (auto_arima)
# - 1つの季節性をモデルに投入することができる

# デメリット:
# - デフォルトでは1つの季節性しか反映することができない (XREGs can help go beyond 1 seasonality)
# - ラグが多すぎると予測が不安定になる
# - パラメータサーチは高コストな処理（auto_arima）


# ＜目次＞
# 0 準備
# 1 コンセプトの整理
# 2 ARIMA
# 3 AUTO ARIMA + XREGS(外部変数)
# 4 モデル保存


# 0 準備 -------------------------------------------------------------------------------

# * LIBRARIES & SETUP -------------------------------------------------

# Time Series ML
library(tidymodels)
library(modeltime)

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

# 訓練データ
train_tbl <-
  splits %>%
    training() %>%
    select(optin_time, optins_trans)

# データ確認
train_tbl %>% print()


# 1 コンセプトの整理 ------------------------------------------------------------------

# ＜目次＞
# * ARモデル
# * 1期間の予測
# * 複数期間の予測（再帰的）
# * Integration (I)
# * ARIMA (MA) = Modeling Errors
# * Seasonal ARIMA
# * SARIMAX - Seasonal Regression w/ ARIMA Errors


# * ARモデル ----------------------------------------------------

# ＜ポイント＞
# - AR(Automated Regression)モデルは基本的にラグ系列を追加した線形回帰と同じ
# - ARIMAモデルでもorder引数の(p, d, q)のうちdとqを0とすることで表現することができる

# ヘルプ確認
?ar
?arima

# ARモデル
# --- 自己回帰モデル
# --- stats::ar()
# --- order.maxで回帰係数の数が決まる
train_tbl$optins_trans %>% ar(order.max = 3)

# ARIMAモデル
# --- 自己回帰和分移動平均モデル
# --- stats::arima()
fit_arima_ar <- train_tbl$optins_trans %>% arima(order = c(3, 0, 0))
fit_arima_ar %>% print()

# 計算証明
# --- 線形回帰から再現しているが完全一致はしていない
# --- ラグ2又はラグ3の結果と一致
fit_lm_ar_2 <-
   lm(optins_trans ~ lag_vec(optins_trans, 1)
                   + lag_vec(optins_trans, 2),
                   data = train_tbl)

fit_lm_ar_3 <-
   lm(optins_trans ~ lag_vec(optins_trans, 1)
                   + lag_vec(optins_trans, 2)
                   + lag_vec(optins_trans, 3),
                   data = train_tbl)

# 確認
fit_lm_ar_2 %>% tidy()
fit_lm_ar_3 %>% tidy()


# * 1期間の予測 ---------------------------------------------------------------

# ＜ポイント＞
# - predict()に整数値を与えることで予測期間を指定することができる
#   --- 以下では1期間
#   --- 予測に必要なorder数だけのXの値が必要

# 予測値の計算
# --- ARIMAモデル（3期間でモデリング）
# --- 1期間先を予測
fit_arima_ar %>% predict(1) %>% as_tibble()

# データ確認
# --- 最終日： 2020-01-06
train_tbl %>% tail()

# 予測値の計算
# --- 回帰モデル
# --- 直近3期間のデータが必要（2020-01-04 to 2020-01-06 + 1day）
fit_lm_ar_3  %>%
    predict(newdata = tibble(optins_trans = c(0.214, 0.968, 1.71, NA)))


# * 複数期間の予測（再帰的） -----------------------------------------

# ＜ポイント＞
# - predict()に整数値を与えることで予測期間を指定することができる
#   --- 以下では3期間
#   --- 予測値をインプットとして再帰的に使用している点に注意


# 予測値の計算
# --- ARIMAモデル
# --- 3期間先を予測
fit_arima_ar %>% predict(3) %>% as_tibble()

# 予測値の計算
# --- 期間をずらして予測
fit_lm_ar %>% predict(newdata = tibble(optins_trans = c(0.214, 0.968, 1.71, NA)))
fit_lm_ar %>% predict(newdata = tibble(optins_trans = c(0.968, 1.71, 0.911, NA)))
fit_lm_ar %>% predict(newdata = tibble(optins_trans = c(1.71, 0.911, 0.771, NA)))


# * Integration (I) ------------------------------------------------

# ＜ポイント＞
# - ARIMAモデルのI(Integration)は差分(d)を表している
#   --- order引数の(p, d, q)の｢d｣がIntegrationでコントロールされる個所
#   --- 差分を取ることで非定常過程を定常過程に変換することを目指している


# 差分系列の作成
# --- 原系列を上書き
train_diff_tbl <-
  train_tbl %>%
    mutate(optins_trans = diff_vec(optins_trans, lag = 1, difference = 1)) %>%
    drop_na()

# 確認
train_diff_tbl %>% print()

# モデル構築
# --- 原系列にdを追加(1期間の差分)
# --- 差分系列
# --- 上記2つが同じ結果となることを確認
train_tbl$optins_trans %>% arima(order = c(1, 1, 0))
train_diff_tbl$optins_trans %>% arima(order = c(1, 0, 0))

# 計算証明
# --- 原系列をラグ系列で回帰することで再現できる
lm(optins_trans ~ lag_vec(optins_trans, 1), data = train_diff_tbl)


# * ARIMA (MA) = Modeling Errors ------------------------------------

#
train_tbl$optins_trans %>% arima(order = c(1, 0, 1))


fit_lm_ar1 <- lm(optins_trans ~ lag_vec(optins_trans, 1), data = train_tbl)
fit_lm_ar1

fitted_values_vec <- fit_lm_ar1 %>% predict() %>% as.numeric()

train_error_tbl <-
  train_tbl %>%
    slice(-1) %>%
    mutate(error = optins_trans - fitted_values_vec)

lm(optins_trans ~ lag_vec(optins_trans, 1) 
   + lag_vec(error, 1), 
   data = train_error_tbl)


# * Seasonal ARIMA ------------------------------------------------

# ARIMAモデル
# --- ARIMAに季節性を反映
# --- seasonal引数に季節性を表現
train_tbl$optins_trans %>%
    arima(order    = c(1, 0, 0),
          seasonal = list(order  = c(2, 0, 0),
                          period = 7))

# 計算証明
# --- 回帰モデルで季節性の項のみ表現
lm(optins_trans ~ lag_vec(optins_trans, 1)
                + lag_vec(optins_trans, 7)
                + lag_vec(optins_trans, 14),
   data = train_tbl)


# * SARIMAX - Seasonal Regression w/ ARIMA Errors ----------------

# ARIMAモデル
# --- ARIMAに季節性と外部変数を反映
train_tbl$optins_trans %>%
  arima(order    = c(1, 0, 0),
        seasonal = list(order  = c(2, 0, 0),
                        period = 7),
        xreg = matrix(month(train_tbl$optin_time)))

# 計算証明
# --- 回帰モデルで季節性の項と外部変数を表現
lm(optins_trans ~ lag_vec(optins_trans, 1)
                + lag_vec(optins_trans, 7)
                + lag_vec(optins_trans, 14)
                + month(optin_time),
                data = train_tbl)


# 2 ARIMAモデル --------------------------------------------------------------

# ＜ポイント＞
# - modeltimeのARIMAモデルのインターフェースを確認する
#   --- {parsnip}と同じ構造となっている（回帰モードのみ）
#   --- {forecast}のARIMA()のラッパー


# ヘルプ参照
?arima_reg()

# モデル構築＆学習
# --- ARIMAモデル
model_fit_arima <-
  arima_reg(seasonal_period = 7,
            non_seasonal_ar = 1,
            non_seasonal_differences = 1,
            non_seasonal_ma = 1,
            seasonal_ar = 1,
            seasonal_differences = 1,
            seasonal_ma = 1) %>%
    set_engine("arima") %>%
    fit(optins_trans ~ optin_time, training(splits))


# 確認
model_fit_arima %>% print()

# プロット作成
# --- 学習済モデルをモデルテーブルに登録
# --- 検証用データの作成(Calibrate)
# --- 予測データの作成
modeltime_table(model_fit_arima) %>%
  modeltime_calibrate(testing(splits)) %>%
  modeltime_forecast(new_data = testing(splits),
                     actual_data = data_prepared_tbl) %>%
  plot_modeltime_forecast()


# 3 AUTO ARIMA + XREGS(外部変数) --------------------------------------------------

# ＜ポイント＞
# - Auto ARIMA


# モデル構築＆学習
# --- Auto ARIMAモデル
model_fit_auto_arima <-
  arima_reg() %>%
    set_engine("auto_arima") %>%
    fit(optins_trans ~ optin_time
        + fourier_vec(optin_time, period = 7)
        + fourier_vec(optin_time, period = 14)
        + fourier_vec(optin_time, period = 30)
        + fourier_vec(optin_time, period = 90)
        + month(optin_time, label = TRUE)
        + lab_event,
        data = training(splits))

# 検証データの作成
# --- 学習済モデルをモデルテーブルに登録
# --- 検証データから検証用の予測値を作成
calibration_tbl <-
  modeltime_table(model_fit_arima,
                  model_fit_auto_arima) %>%
    modeltime_calibrate(testing(splits))

# プロット作成
# --- Actualデータ+予測データ
calibration_tbl %>%
  modeltime_forecast(new_data = testing(splits),
                     actual_data = data_prepared_tbl) %>%
  plot_modeltime_forecast()

# モデル精度の評価
calibration_tbl %>% modeltime_accuracy()

# * Refit ----

refit_tbl <-
  calibration_tbl %>%
    modeltime_refit(data_prepared_tbl)

refit_tbl %>%
    modeltime_forecast(new_data = artifacts_list$data$forecast_tbl,
                       actual_data = data_prepared_tbl) %>%
    plot_modeltime_forecast(.conf_interval_alpha = 0.05)


# 4 モデル保存 ----------------------------------------------------------------

# モデル抽出
# model_fit_best_arima <-
#   calibration_tbl %>%
#     slice(2) %>%
#     pull(.model) %>%
#     pluck(1)

# モデル保存
# model_fit_best_arima %>% write_rds("00_models/model_fit_best_arima.rds")
# read_rds("00_models/model_fit_best_arima.rds")

# ****************************************************************************
# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: ADVANCED TIME SERIES FORECASTING FOR BUSINESS
# MODULE: TIME SERIES JUMPSTART
# ****************************************************************************

# ＜ゴール＞
# GOAL: Forecast Daily Email Users - Next 8-WEEKS


# ＜目的＞
# - 時系列分析のプロジェクトをやってみる
# - {modeltime}のフレームワークを体験する
# - 以下の2つの時系列アルゴリズムを使ってみる
#   1. Prophet
#   2. LM w/ Engineered Features


# 0.0 準備 --------------------------------------------------------------------

# * ライブラリ ----------------------------

# Time Series Machine Lerning
library(tidymodels)
library(modeltime)

# EDA
library(DataExplorer)

# Core
library(tidyverse)
library(timetk)
library(lubridate)


# * データ準備 ----------------------------

# インポート
mailchimp_users_tbl <- read_rds("00_data/mailchimp_users.rds")


# 1.0 EDA & データ加工 -----------------------------------------------------------

# * DAILY SUBSCRIBERS INCREASES

# * データ確認 --------------------------------------------

# データ概要
mailchimp_users_tbl %>% glimpse()
mailchimp_users_tbl %>% print()


# * 時系列サマリー ----

# optinsを日付ごとにカウントする
# --- .by : 時系列フレーズが使える
optins_day_tbl <-
  mailchimp_users_tbl %>%
    summarise_by_time(.date_var = optin_time,
                      .by = "day",
                      optins = n())


# 確認
# --- dayで見た場合は日付の欠損がある
optins_day_tbl %>% print()


# * 日付サマリー -----------------------------------------

# 日付インデックスのサマリー
# --- データフレームから日付インデックスを直接取得する
# --- 1day = 86400sec
optins_day_tbl %>% tk_summary_diagnostics(.date_var = optin_time)


# 別の方法
# --- 日付インデックスを取得してからサマリー
optins_day_tbl %>%
  tk_index() %>%
  tk_get_timeseries_summary()


# * 日付の補完 ------------------------------------------

# データ確認
# --- 最初に25日間の日付欠損がある
optins_day_tbl %>%
  select(-optins) %>%
  mutate(lag = lag(optin_time),
         diff = optin_time - lag)


# 欠損した日付補完
optins_day_prepared_tbl <-
  optins_day_tbl %>%
    pad_by_time(.date_var = optin_time,
                .by = "day",
                .pad_value = 0)


# 確認
optins_day_prepared_tbl %>% print()


# * 可視化 ----------------------------------------------

# プロット作成
optins_day_prepared_tbl %>%
  plot_time_series(optin_time, optins)


# 2.0 モデリング期間の設定 ----------------------------------------------

# * フィルタリング ---------------------------------------

# 日付インデックスでフィルタリング
# --- 2018-11-19のスパイクを取り除く
evaluation_tbl <-
  optins_day_prepared_tbl %>%
    filter_by_time(.date_var = optin_time,
                   .start_date = "2018-11-20",
                   .end_date = "end")

# プロット作成
evaluation_tbl %>%
  plot_time_series(optin_time, optins)


# * 訓練データ/テストデータへの分割 ----

# 時系列データ分割
splits <-
  evaluation_tbl %>%
    time_series_split(date_var = optin_time,
                      assess = "8 week",
                      cumulative = TRUE)


# データ確認
splits %>% print()
splits %>% glimpse()


# データ出力
splits %>%
  training() %>%
  tk_summary_diagnostics()
splits %>% testing() %>% tk_summary_diagnostics()


# プロット作成
# --- 分割イメージを可視化
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins)


# 3.0 PROPHETによる予測 ----------------------------------------------

# ＜目的＞
# - {modeltime}のprophet回帰を行ってみる
# - ベースラインモデルとして使用する
#   --- 特徴量エンジニアリングなどの工夫は一切行っていない


# * モデリング ----

# モデル構築
# --- modeltime::prophet_reg()
# --- {parsnip}と同様のフレームワークとなっている
model_prophet_fit <-
  prophet_reg() %>%
    set_engine("prophet") %>%
    fit(optins ~ optin_time, data = training(splits))


# 確認
model_prophet_fit %>% print()


# * {Modeltime}のワークフロー ----

# モデルをテーブルに登録
# --- tibbleにモデルごとにネストして格納される
# --- 複数モデルの一括処理を想定
model_tbl <- modeltime_table(model_prophet_fit)


# 確認
model_tbl %>% print()


# * Calibration(調整) ----

# 予測値の計算
# --- Calibrationとは校正の意味
calibration_tbl <-
  model_tbl %>%
    modeltime_calibrate(new_data = testing(splits))


# 確認
calibration_tbl %>% print()


# * Visualize Forecast ----

# 可視化
# --- evaluation_tblは元の全体データ
calibration_tbl %>%
  modeltime_forecast(actual_data = evaluation_tbl) %>%
  plot_modeltime_forecast()


# * Check Metric ----

# メトリックの計算
calibration_tbl %>% modeltime_accuracy()


# 4.0 FORECASTING WITH FEATURE ENGINEERING ----

# ＜目的＞
# - 季節性に着目した特徴量エンジニアリングを行う
# - セカンダリーモデルとしてlmを導入する
#   --- prophetモデルと比較(特徴量エンジニアリングなし)


# * Identify Possible Features ----

# 季節性の診断プロット
# --- optinsを対数変換
evaluation_tbl %>%
  plot_seasonal_diagnostics(optin_time, log(optins))


# * {Recipes}による特徴量エンジニアリング ----

# データ確認
# --- 訓練データに特徴量エンジニアリングを行う
splits %>% training()


# レシピ作成
# --- ｢Time Series Signature｣の追加
# --- ｢Time Series Signature｣の重複要素と時間単位の項目を削除
recipe_spec <-
  recipe(optins ~ ., data = training(splits)) %>%
    step_timeseries_signature(optin_time) %>%
    step_rm(ends_with(".iso"),
            ends_with(".xts"),
            contains("hour"),
            contains("minute"),
            contains("second"),
            contains("am.pm")) %>%
    step_normalize(ends_with("index.num"),
                   ends_with("_year")) %>%
    step_dummy(all_nominal())


# レシピ確認
# --- データ項目の確認
recipe_spec %>% prep() %>% juice() %>% glimpse()


# レシピ確認
recipe_spec %>% tidy()


# * {workflow}による機械学習 ----

# モデル構築
model_spec <-
  linear_reg() %>%
    set_engine("lm")


# ワークフロー構築
workflow_fit_lm <-
  workflow() %>%
    add_model(model_spec) %>%
    add_recipe(recipe_spec) %>%
    fit(data = training(splits))


# 確認
workflow_fit_lm %>% print()


# * Modeltime Process ----

# モデルをテーブルに登録
calibration_tbl <-
  modeltime_table(model_prophet_fit,
                  workflow_fit_lm) %>%
    modeltime_calibrate(new_data = testing(splits))


# 確認
calibration_tbl %>% print()


# メトリックの計算
calibration_tbl %>% modeltime_accuracy()


# 予測データの作成
calibration_tbl %>%
  modeltime_forecast(new_data = testing(splits),
                     actual_data = evaluation_tbl) %>%
  plot_modeltime_forecast()


# 5.0 SUMMARY & NEXT STEPS ----

# * ここで何を学んだか ----
# - 以下の２つの体験をした:
#   - Tidymodels / Modeltime Framework
# - ２つのモデリングアプローチを確認した:
#   - Prophet - Univariate, Automatic
#   - Linear Regression Model - Many recipe steps
# - 特徴量エンジニアリングの実践を体験した
#   - 可視化: ACF, Seasonality
#   - 日付に着目した特徴量エンジニアリング


# * これからのステップ! ----
# - You still need to learn:
#   - 新しいアルゴリズム
#   - 機械学習 - パラメータチューニングをどのように行うか
#   - 戦略的な特徴量エンジニアリング
#   - アンサンブル - Competition winning strategy
#   - and a lot more!



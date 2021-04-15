# ****************************************************************************
# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: ADVANCED FEATURE ENGINEERING
# ****************************************************************************


# GOAL ----
# - 時系列データの特徴量エンジニアリングをRecipeフレームワークで行う


# OBJECTIVES ----
# - IMPLEMENT RECIPES PIPELINE
# - APPLY MODELTIME WORKFLOW & VISUALIZE RESULTS
# - COMPARE SPLINE MODEL VS LAG MODEL


# ＜目次＞
# 0 準備
# 1 全期間データの作成
# 2 テストデータを取り分け
# 3 時系列データ分割
# 4 レシピ
# 5 スプラインモデル
# 6 modeltimeの処理
# 7 ラグモデル
# 8 将来データの予測



# 0 準備 ------------------------------------------------------------------------

# * Change Locale -----------------------------------------------------

Sys.setlocale("LC_TIME", "English")
Sys.getlocale("LC_TIME")


# ライブラリ -------------------------------------------------------------

# Time Series ML
library(tidymodels)
library(modeltime)

# Core
library(tidyverse)
library(lubridate)
library(timetk)


# * データロード --------------------------------------------------------

# Data
subscribers_tbl   <- read_rds("00_data/mailchimp_users.rds")
learning_labs_tbl <- read_rds("00_data/learning_labs.rds") 


# * データ準備 --------------------------------

# optinsデータ
subscribers_prep_tbl <-
  subscribers_tbl %>%
    summarise_by_time(optin_time, .by = "day", optins = n()) %>%
    pad_by_time(.pad_value = 0)

# データ確認
subscribers_prep_tbl %>% print()


# イベントデータ
learning_labs_prep_tbl <-
  learning_labs_tbl %>%
    mutate(event_date = ymd_hms(event_date)) %>%
    summarise_by_time(event_date, .by = "day", event = n())

# データ確認
learning_labs_prep_tbl %>% print()


# * データ変換 --------------------------------

# Data Transformation
# --- Preprocess Target
# --- Fix missing values at beginning of series
# --- Cleaning
subscribers_transformed_tbl <-
  subscribers_prep_tbl %>%
    mutate(optins_trans = log_interval_vec(optins, limit_lower = 0, offset = 1)) %>%
    mutate(optins_trans = standardize_vec(optins_trans)) %>%
    select(-optins) %>%
    filter_by_time(.start_date = "2018-07-03") %>%
    mutate(optins_trans_cleaned = ts_clean_vec(optins_trans, period = 7)) %>%
    mutate(optins_trans = ifelse(optin_time %>% between_time("2018-11-18", "2018-11-20"), 
                                 optins_trans_cleaned,
                                 optins_trans)) %>%
    select(-optins_trans_cleaned)

# データ確認
subscribers_transformed_tbl %>% print()

# プロット作成
subscribers_transformed_tbl %>%
  plot_time_series(optin_time, optins_trans)

# 期間確認
# --- 最終日：2020-03-02
subscribers_transformed_tbl %>%
  tk_summary_diagnostics() %>%
  glimpse()


# * パラメータ保存 --------------------------------

# log_interval_vec():
limit_lower <- 0
limit_upper <- 3650.8
offset      <- 1

# Standardization Parameters
std_mean    <- -5.25529020756467
std_sd      <- 1.1109817111334



# 1 全期間データの作成 ----------------------------------------------------

# ＜ポイント＞
# - Extend to Future Window
# - Add any lags to full dataset
# - Add any external regressors to full dataset


# パラメータ設定
horizon    <- 8 * 7
lag_period <- 8 * 7
rolling_periods <- c(30, 60, 90)

# 将来データの作成
data_prepared_future_tbl <-
  subscribers_transformed_tbl %>%
    future_frame(.date_var = optin_time, .length_out = horizon)

# データ作成
# --- 将来日付の追加（56日間）
# --- ラグ系列の追加（56日ラグ）
# --- ローリング平均の追加（30D, 60D, 90D）
# --- イベントの追加（1/0フラグで表現）
# --- 列の整理（dplyr::rename_with()は複数の列名修正を関数を用いて行う）
data_prepared_full_tbl <-
  subscribers_transformed_tbl %>%
    bind_rows(data_prepared_future_tbl) %>%
    tk_augment_lags(optins_trans, .lags = lag_period) %>%
    tk_augment_slidify(.value   = optins_trans_lag56,
                       .f       = mean,
                       .period  = rolling_periods,
                       .align   = "center",
                       .partial = TRUE) %>%
    left_join(learning_labs_prep_tbl, by = c("optin_time" = "event_date")) %>%
    mutate(event = ifelse(is.na(event), 0, event)) %>%
    rename(lab_event = event) %>%
    rename_with(.cols = contains("lag"), .fn = ~ str_c("lag_", .))

# プロット作成
data_prepared_full_tbl %>%
    pivot_longer(-optin_time) %>%
    group_by(name) %>%
    plot_time_series(optin_time, value, name, .smooth = FALSE)

# データ確認
# --- 将来データ + 1日
data_prepared_full_tbl %>% tail(8*7 + 1)



# 2 テストデータを取り分け ----------------------------------------------

# ＜ポイント＞
# - 手動でデータ分割

# モデル構築用データ
# --- 訓練データ + 検証データ
# --- optins_transがNAでないデータ
data_prepared_tbl <-
  data_prepared_full_tbl %>%
    filter(!is.na(optins_trans))

# データ確認
data_prepared_tbl %>% print()

# テストデータ
# --- 将来データ
# --- optins_transがNAのデータ
forecast_tbl <-
  data_prepared_full_tbl %>%
    filter(is.na(optins_trans))

# データ確認
forecast_tbl %>% print()



# 3 時系列データ分割 --------------------------------------------------

# ＜ポイント＞
# - 関数を用いてデータ分割


# データ確認
data_prepared_tbl %>% print()

# データ分割
# --- 検証データ= 56
splits <-
  data_prepared_tbl %>%
    time_series_split(assess = horizon, cumulative = TRUE)

# CVプランの確認
# --- ｢.id｣｢.key｣の列が追加されている
splits %>%
  tk_time_series_cv_plan() %>%
  glimpse()

# プロット作成
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(optin_time, optins_trans)



# 4 レシピ ----------------------------------------------------------------

# ＜ポイント＞
# - Time Series Signature - Adds bulk time-based features
# - Spline Transformation to index.num
# - Interaction: wday.lbl:week2
# - Fourier Features


# モデルのロード
model_fit_best_lm <- read_rds("00_models/model_fit_best_lm.rds")

# モデル確認
model_fit_best_lm %>% summary()

# フォーミュラ確認
model_fit_best_lm$terms %>% formula()


# レシピ作成
# --- 各ステップで実行して｢データ確認｣で出力を確認！！
# --- 訓練データを元に定義
# --- (A)|(B) はReglexでAorBの意味
recipe_spec_base <-
  recipe(optins_trans ~ ., data = training(splits)) %>%
    step_timeseries_signature(optin_time) %>%
    step_rm(matches("(iso)|(xts)|(hour)|(minute)|(second)|(am.pm)")) %>%
    step_normalize(matches("(index.num)|(year)|(yday)")) %>%
    step_dummy(all_nominal(), one_hot = TRUE) %>%
    step_interact(~ matches("week2") * matches("wday.lbl")) %>%
    step_fourier(optin_time, period = c(7, 14, 30, 90, 365), K = 2)


# データ確認
# --- 元データ
splits %>% training() %>% glimpse()

# データ確認
# --- レシピ適用後のデータ
recipe_spec_base %>% prep() %>% juice() %>% glimpse()



# 5 スプラインモデル ----------------------------------------------------------

# ＜ポイント＞
# - ナチュラル・スプラインをレシピに追加


# * モデルパーツ -----------------------------------------------

# モデル構築
# --- 線形回帰モデル
model_spec_lm <-
  linear_reg() %>%
    set_engine("lm")

# レシピ追加
# --- ナチュラル・スプライン
recipe_spec_1 <-
  recipe_spec_base %>%
    step_rm(optin_time) %>%
    step_ns(ends_with("index.num"), deg_free = 2) %>%
    step_rm(starts_with("lag_"))

# データ確認
# --- レシピ適用後のデータ
recipe_spec_1 %>% prep() %>% juice() %>% glimpse()


# * ワークフロー設定  -----------------------------------------

# ワークフローの設定
# --- 学習も同時に実施
workflow_fit_lm_1_spline <-
  workflow() %>%
    add_model(model_spec_lm) %>%
    add_recipe(recipe_spec_1) %>%
    fit(training(splits))

# 結果確認
workflow_fit_lm_1_spline %>% print()

# モデル確認
workflow_fit_lm_1_spline %>%
  pull_workflow_fit() %>%
  pluck("fit") %>%
  summary()


# 6 modeltimeの処理 --------------------------------------------------------

# モデル登録
# --- 学習済モデルをテーブルに追加
# --- モデルに基づいて予測
calibration_tbl <-
  workflow_fit_lm_1_spline %>%
    modeltime_table() %>%
    modeltime_calibrate(new_data = testing(splits))

# モデル精度の確認
calibration_tbl %>% modeltime_accuracy()

# プロット作成
# --- プロット用の予測データの作成
calibration_tbl %>%
    modeltime_forecast(new_data    = testing(splits), 
                       actual_data = data_prepared_tbl) %>%
    plot_modeltime_forecast()


# 7 ラグモデル ---------------------------------------------------------------

# * レシピ追加 ------------------------------------------------

# データ確認
# --- ラグ系列は最初のほうがNAとなっている
recipe_spec_base %>% prep() %>% juice() %>% glimpse()

# レシピ追加
# --- ラグ系列のNAを削除
recipe_spec_2 <-
  recipe_spec_base %>%
    step_rm(optin_time) %>%
    step_naomit(starts_with("lag_"))
    
# データ確認
recipe_spec_2 %>% prep() %>% juice() %>% glimpse()

# 日付確認
recipe_spec_base %>% prep() %>% juice() %>% tk_summary_diagnostics()


# * Lag Workflow ---------------------------------------------

# ワークフローの設定
# --- 学習も同時に行っている
workflow_fit_lm_2_lag <-
  workflow() %>%
    add_model(model_spec_lm) %>%
    add_recipe(recipe_spec_2) %>%
    fit(training(splits))

# 結果確認
workflow_fit_lm_2_lag %>% print()

# モデル確認
# --- purrr::pluck()
workflow_fit_lm_2_lag %>%
  pull_workflow_fit() %>%
  pluck("fit") %>%
  summary()


# * modeltimeで比較 ----------------------------------------

# モデル登録
# --- 学習済モデルをテーブルに追加
# --- モデルに基づいて予測
calibration_tbl <-
  modeltime_table(workflow_fit_lm_1_spline,
                  workflow_fit_lm_2_lag) %>%
    modeltime_calibrate(new_data = testing(splits))

# モデル精度の確認
calibration_tbl %>% modeltime_accuracy()

# プロット作成
# --- プロット用の予測データの作成
calibration_tbl %>%
    modeltime_forecast(new_data    = testing(splits), 
                       actual_data = data_prepared_tbl) %>%
    plot_modeltime_forecast()


# 8 将来データの予測 ----------------------------------------------------------------------------

# データ確認
# --- 前処理をしていない元データ
data_prepared_tbl %>% print()

# モデルの再学習
# --- 共通データを用いて再度学習
refit_tbl <-
  calibration_tbl %>%
    modeltime_refit(data = data_prepared_tbl)

# プロット確認
# --- 予測データをデータフレームに変換
# --- 対数変換基準化(元データの平均と標準偏差を使用)
# --- 対数を再変換（元データに戻る）
refit_tbl %>%
    modeltime_forecast(new_data    = forecast_tbl,
                       actual_data = data_prepared_tbl) %>%
    
    # Invert Transformation
    mutate(across(.value:.conf_hi, .fns = ~ standardize_inv_vec(
        x    = .,
        mean = std_mean,
        sd   = std_sd
    ))) %>%
    mutate(across(.value:.conf_hi, .fns = ~ log_interval_inv_vec(
        x           = ., 
        limit_lower = limit_lower, 
        limit_upper = limit_upper, 
        offset      = offset
    ))) %>%
    
    plot_modeltime_forecast()

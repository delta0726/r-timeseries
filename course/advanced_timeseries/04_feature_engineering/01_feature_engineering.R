# ****************************************************************************
# BUSINESS SCIENCE UNIVERSITY
# DS4B 103-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: FEATURE ENGINEERING
# ****************************************************************************


# GOAL ----
# - 時系列データの特徴量エンジニアリングのテクニックを習得する
# - Recipeは使用しない


# ＜目次＞
# 0 準備
# 1 時間ベースの前処理
# 2 相互効果
# 3 フーリエ系列
# 4 ラグ処理
# 5 イベント処理
# 6 外部変数を用いたモデリング
# 7 RECOMMENDATION


# 0 準備 ------------------------------------------------------------------------

# * Change Locale -----------------------------------------------------

Sys.setlocale("LC_TIME", "English")
Sys.getlocale("LC_TIME")


# ライブラリ -----------------------------------------------------------------

# Core
library(tidyverse)
library(lubridate)
library(timetk)
library(plotly)


# * データロード -------------------------------

# データ1
google_analytics_summary_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")
google_analytics_summary_tbl %>% print()

# データ2
learning_labs_tbl <- read_rds("00_data/learning_labs.rds") 
learning_labs_tbl %>% print()

# データ3
subscribers_tbl   <- read_rds("00_data/mailchimp_users.rds")
subscribers_tbl %>% print()


# * データ準備 --------------------------------

# データ加工
data_prepared_tbl <-
  subscribers_tbl %>%
    summarise_by_time(optin_time, .by = "day", optins = n()) %>%
    pad_by_time(.pad_value = 0) %>%
    
    # Preprocessing
    mutate(optins_trans = log_interval_vec(optins, limit_lower = 0, offset = 1)) %>%
    mutate(optins_trans = standardize_vec(optins_trans)) %>%
    
    # Fix missing values at beginning of series
    filter_by_time(.start_date = "2018-07-03") %>%
    
    # Cleaning
    mutate(optins_trans_cleaned = ts_clean_vec(optins_trans, period = 7)) %>%
    mutate(optins_trans = ifelse(optin_time %>% between_time("2018-11-18", "2018-11-20"), 
                                 optins_trans_cleaned,
                                 optins_trans)) %>%
    
    select(-optins, -optins_trans_cleaned)



# プロット確認
data_prepared_tbl %>%
  pivot_longer(contains("trans")) %>%
  plot_time_series(optin_time, value, name)


# 1 時間ベースの前処理 ----------------------------------------

# ＜サブタイトル＞
# * Time Series Signature
# * トレンド系列の作成
# * 季節性


# * Time Series Signature --------------------------------

# ＜ポイント＞
# - 日付インデックスを28の日付要素に分解する
#   --- tk_augment_timeseries_signature()
#   --- 日付でモデリングする際に必要となる日付要素を一括取得


# 日付情報の追加
# --- Time Series Signature
data_prep_signature_tbl <-
  data_prepared_tbl %>%
    tk_augment_timeseries_signature() %>%
    select(-diff,
           -ends_with("iso"),
           -ends_with(".xts"),
           -contains("hour"),
           -contains("minute"),
           -contains("second"),
           -contains("am.pm"))

# 確認
data_prep_signature_tbl %>% glimpse()


# * トレンド系列の作成 --------------------------------

# ＜ポイント＞
# - 元系列のトレンド系列は説明変数として有効
# - 線形/非線形で捉えることが可能


# 線形トレンドの追加
data_prep_signature_tbl %>%
  plot_time_series_regression(.date_var = optin_time,
                              .formula = optins_trans ~ index.num)

# 非線形トレンドの追加
# --- B-Spline
data_prep_signature_tbl %>%
  plot_time_series_regression(
    .date_var = optin_time,
    .formula = optins_trans ~ splines::bs(index.num, df = 3),
    .show_summary = TRUE
        )

# 非線形トレンドの追加
# --- Natural-Spline
data_prep_signature_tbl %>%
  plot_time_series_regression(
    .date_var = optin_time,
    .formula = optins_trans ~ splines::ns(index.num,
                                          knots = quantile(index.num, probs = c(0.25, 0.5))),
    .show_summary = TRUE
        )


# * 季節性 ------------------------------------------

# 週次の季節性
# --- 線形トレンドの追加(wday)
data_prep_signature_tbl %>%
    plot_time_series_regression(
        .date_var = optin_time,
        .formula = optins_trans ~ wday.lbl,
        .show_summary = TRUE
    )

# 月次の季節性
# --- 線形トレンドの追加(month)
data_prep_signature_tbl %>%
    plot_time_series_regression(
        .date_var = optin_time,
        .formula = optins_trans ~ month.lbl,
        .show_summary = TRUE
    )

# トレンドの結合
# --- フォーミュラ定義
# --- Natural-Spline + 週次トレンド + 月次トレンド
model_fomula_seasonality <-
  as.formula(
    optins_trans ~ splines::ns(index.num,
                               knots = quantile(index.num, probs = c(0.25, 0.5)))
    + wday.lbl + month.lbl + .
)

# プロット作成
# --- 非線形トレンドの追加
# --- フォーミュラを適用
data_prep_signature_tbl %>%
    plot_time_series_regression(
        .date_var = optin_time,
        .formula = model_fomula_seasonality,
        .show_summary = TRUE
    )


# 2 相互効果 --------------------------------------------------------------------

# データ確認
data_prep_signature_tbl %>% glimpse()

# フォーミュラ定義
# --- Natural Spline
# --- 相互効果： as.factor(week2) * wday.lbl
model_formula_interactions <-
  as.formula(
    optins_trans ~ splines::ns(index.num,
                               knots = quantile(index.num, probs = c(0.25, 0.5)))
    + .
    + (as.factor(week2) * wday.lbl)
)

# プロット作成
data_prep_signature_tbl %>%
    plot_time_series_regression(
        optin_time, 
        .formula = model_formula_interactions,
        .show_summary = TRUE
    )


# 3 フーリエ系列 ---------------------------------------------------------------------

# プロット作成
# --- ACFとPACFで自己相関を確認
data_prep_signature_tbl %>%
  plot_acf_diagnostics(optin_time, optins_trans)

# データ作成
# --- フーリエ系列の追加
data_prep_fourier_tbl <-
  data_prep_signature_tbl %>%
    tk_augment_fourier(optin_time, .periods = c(7, 14, 30, 90, 365), .K = 2)

# データ確認
data_prep_fourier_tbl %>% glimpse()

# フォーミュラ定義
# --- Natural Spline
# --- フーリエ系列
# --- 相互効果： as.factor(week2) * wday.lbl
model_formula_fourier <- as.formula(
    optins_trans ~ splines::ns(index.num,
                               knots = quantile(index.num, probs = c(0.25, 0.5)))
    + .
    + (as.factor(week2) * wday.lbl)
)

# プロット作成
data_prep_fourier_tbl %>%
    # filter_by_time(.start_date = "2018-09-13") %>%
    plot_time_series_regression(
        optin_time,
        .formula = model_formula_fourier,
        .show_summary = TRUE
    )


# 4 ラグ処理 -----------------------------------------------------------------

# ACFの確認
# --- ラグ処理による自己相関を確認
data_prep_fourier_tbl %>%
  plot_acf_diagnostics(optin_time,
                       .value = optins_trans,
                       .lags = (8*7 + 1):600)

# データ作成
# --- フーリエ系列を含むデータセットを使用（3で作成）
# --- ラグ系列の追加
data_prep_lags_tbl <-
  data_prep_fourier_tbl %>%
    tk_augment_lags(optins_trans, .lags = c(57, 63, 70)) %>%
    drop_na()

# データ確認
data_prep_lags_tbl %>% glimpse()

# フォーミュラ定義
# --- Natural Spline
# --- フーリエ系列
# --- ラグ系列
# --- 相互効果： as.factor(week2) * wday.lbl
model_formula <- as.formula(
    optins_trans ~ splines::ns(index.num, knots = quantile(index.num, probs = c(0.25)))
    + .
    + (as.factor(week2) * wday.lbl)
)

# プロット作成
data_prep_lags_tbl %>%
  plot_time_series_regression(optin_time,
                              .formula = model_formula,
                              .show_summary = TRUE)


# 5 イベント処理 ----------------------------------------------------

# データ作成
learning_labs_daily_tbl <-
  learning_labs_tbl %>%
    mutate(event_date = ymd_hms(event_date)) %>%
    summarise_by_time(event_date, .by = "day", event = n())

# イベント定義
# --- フーリエ系列を含むデータセットを使用（3で作成）
# --- 最後の列にイベント列を追加
data_prep_events_tbl <-
  data_prep_fourier_tbl %>%
    left_join(learning_labs_daily_tbl, by = c("optin_time" = "event_date")) %>%
    mutate(event = ifelse(is.na(event), 0, event))

# データ確認
data_prep_events_tbl %>% glimpse()

# プロット作成
# --- イベントを赤のポイントで表示
g <-
  data_prep_events_tbl %>%
    plot_time_series(optin_time, optins_trans, .interactive = FALSE) +
    geom_point(color = "red", data = . %>% filter(event == 1))

# プロット確認
ggplotly(g)

# フォーミュラ定義
# --- Natural Spline
# --- フーリエ系列
# --- イベント情報も追加
# --- 相互効果： as.factor(week2) * wday.lbl
model_formula <- as.formula(
    optins_trans ~ splines::ns(index.num, knots = quantile(index.num, probs = c(0.25, 0.40)))
    + .
    + (as.factor(week2) * wday.lbl)
)


# プロット作成
data_prep_events_tbl %>%
    plot_time_series_regression(
        optin_time,
        .formula = model_formula,
        .show_summary = TRUE
    )


# 6 外部変数を用いたモデリング --------------------------------

# ＜ポイント＞
# - 時系列分析は定常過程を想定することから元系列から派生したデータで予測することが多い
# - 外部変数(External Regressors)を追加することも可能


# データ準備
# --- ページ情報を日次で集計
# --- 対数変換してからZスコア変換
# --- 元データに後ほど結合する
google_analytics_prep_tbl <-
  google_analytics_summary_tbl %>%
    mutate(date = ymd_h(dateHour)) %>%
    summarise_by_time(date, .by = "day", across(pageViews:sessions, .fns = sum)) %>%
    mutate(across(pageViews:sessions, .fns = log1p),
           across(pageViews:sessions, .fns = standardize_vec),
           pageViews_lag63 = lag_vec(pageViews, lag = 63))

# データ確認
google_analytics_prep_tbl %>% print()
google_analytics_prep_tbl %>% glimpse()

# データ結合
# --- 5で作成したデータを起点とする
data_prep_google_tbl <-
  data_prep_events_tbl %>%
    left_join(google_analytics_prep_tbl, by = c("optin_time" = "date")) %>%
    drop_na()

# データ確認
data_prep_google_tbl %>% glimpse()

# プロット作成
# --- CCFを確認
data_prep_google_tbl %>%
    plot_acf_diagnostics(
        optin_time, optins_trans,
        .ccf_vars = pageViews:sessions,
        .show_ccf_vars_only = TRUE
    )

# フォーミュラ定義
# --- Natural Spline
# --- 相互効果： as.factor(week2) * wday.lbl
model_formula <- as.formula(
    optins_trans ~ splines::ns(index.num, knots = quantile(index.num, probs = c(0.25, 0.40)))
    + .
    + (as.factor(week2) * wday.lbl) 
    - pageViews - organicSearches - sessions
    + pageViews_lag63
)

# プロット作成
# --- 時系列回帰
data_prep_google_tbl %>%
    plot_time_series_regression(
        .date_var = optin_time,
        .formula = model_formula,
        .show_summary = TRUE
    )

# プロット比較
data_prep_google_tbl %>%
    select(optin_time, optins_trans, pageViews) %>%
    pivot_longer(-optin_time) %>%
    plot_time_series(optin_time, value, name, .smooth = FALSE)


# 7 RECOMMENDATION --------------------------------------------------------------------

# - Best model:
# - Best Model Formula:


# フォーミュラ定義
# --- Natural Spline
# --- 相互効果： as.factor(week2) * wday.lbl
model_formula <- as.formula(
    optins_trans ~ splines::ns(index.num, knots = quantile(index.num, probs = c(0.25, 0.40)))
    + .
    + (as.factor(week2) * wday.lbl)
)

# プロット作成
data_prep_events_tbl %>%
    plot_time_series_regression(
        optin_time,
        .formula = model_formula,
        .show_summary = TRUE
    )


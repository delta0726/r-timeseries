# ****************************************************************************
# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: ADVANCED TIME SERIES FORECASTING FOR BUSINESS
# MODULE: VISUALIZATION TOOLS ----
# ****************************************************************************


# GOAL:
# - plot_time_series()で簡単に時系列プロットを作成する
# - ACF/季節性/アノマリー/季節生分解をプロットで直観的に理解する
# - 時系列回帰をプロットして直観的に理解する


# ＜目次＞
# 0 準備
# 1 時系列プロット
# 2 ACF診断
# 3 季節性
# 4 アノマリー
# 5 季節性分解
# 6 時系列回帰プロット


# 0 準備 --------------------------------------------------------------------

# ローケール設定
Sys.setlocale("LC_TIME", "English")
Sys.getlocale("LC_TIME")

# ライブラリ
library(tidyverse)
library(timetk)
library(lubridate)


# データ準備
# --- データ1
google_analytics_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")
google_analytics_tbl %>% print()

# データ準備
# --- データ2
mailchimp_users_tbl <- read_rds("00_data/mailchimp_users.rds")
mailchimp_users_tbl %>% print()


# * データ加工 : データ1 ----------------------------

# ロング型に変換
google_analytics_long_hour_tbl <-
  google_analytics_tbl %>%
    mutate(date = ymd_h(dateHour)) %>%
    select(-dateHour) %>%
    pivot_longer(cols = pageViews:sessions)

# 確認
google_analytics_long_hour_tbl %>% print()


# * データ加工 : データ2 ----------------------------

# データ確認
# --- 観測日に抜けている日がある
# --- 1日に複数の観測値がある
mailchimp_users_tbl %>%
  group_by(optin_time) %>%
  tally()

# 日付インデックスで集計
# --- 日次で件数を計算
# --- 欠損日付を補完
subscribers_day_tbl <-
  mailchimp_users_tbl %>%
    summarise_by_time(.date_var = optin_time,
                      .by       = "day",
                      optins    = n()) %>%
    pad_by_time(.by = "day", .pad_value = 0)

# 確認
subscribers_day_tbl %>% print()


# 1 時系列プロット --------------------------------------------------------

# ＜ポイント＞
# - 時系列プロットを簡単に作成するための関数
#   --- 本コースで多用

# ＜サブタイトル＞
# * 基本的なプロット
# * グループをカラー/ファセットで表示
# * データ変換：対数変換
# * スムージング
# * グループごとにスムージング
# * その他の設定


# * 基本的なプロット --------------------------------------------

# データ1
subscribers_day_tbl %>%
  plot_time_series(.date_var = optin_time, .value = optins)

# データ2
google_analytics_long_hour_tbl %>%
  plot_time_series(.date_var = date, .value = value)


# * グループをカラー/ファセットで表示 --------------------------------

# ＜ポイント＞
# - 複数系列の表示にはカラー/ファセットで対応する
# - グループ化した状態でplot_time_series()を適用するとファセットが自動で適用される


# グループ件数の確認
# --- データ2
# --- 3種類の系列が格納されている
google_analytics_long_hour_tbl %>%
  group_by(date) %>%
  tally()

# グループをカラー表示
# --- データ2
google_analytics_long_hour_tbl %>%
  plot_time_series(.date_var = date, .value = value, .color_var = name)


# グループをファセット表示
# --- .facet_vars引数を指定
google_analytics_long_hour_tbl %>%
    # group_by(name) %>%
    plot_time_series(.date_var = date, .value = value, .facet_vars = name)

# グループをファセット表示
# --- グループ化
google_analytics_long_hour_tbl %>%
    group_by(name) %>%
    plot_time_series(.date_var = date, .value = value)


# * データ変換：対数変換 ------------------------------------

# ＜ポイント＞
# - 大きい数値が含まれる場合には対数変換を用いる
# - 対数変換する際は正の値にしてから適用
#   --- 1以上にシフト


# 対数変換(変換前)
# --- データ1
# --- optinsは回数を示すデータなので最低がゼロ
subscribers_day_tbl %>%
  plot_time_series(.date_var = optin_time, .value = optins)

# 対数変換(変換後)
# --- データ1
# --- 0-1は対数変換できないので、+1で1以上の数値となるようにする
subscribers_day_tbl %>%
  plot_time_series(.date_var = optin_time, .value = log(optins + 1))

# 対数変換(変換前)
# --- データ2
# --- optinsは回数を示すデータなので最低がゼロ
google_analytics_long_hour_tbl %>%
  group_by(name) %>%
  plot_time_series(.date_var = date,
                   .value    = value)

# 対数変換(変換後)
# --- データ2
google_analytics_long_hour_tbl %>%
  group_by(name) %>%
  plot_time_series(.date_var = date,
                   .value    = log(value + 1))


# * スムージング --------------------------------------------

# ＜ポイント＞
# - plot_time_series()でLOESSスムージングを表示する


# プロット作成
# --- スムージング表示なし
# --- geom_smooth()と同様
subscribers_day_tbl %>%
  plot_time_series(.date_var = optin_time,
                   .value = log(optins + 1),
                   .smooth = FALSE)

# プロット作成
# --- スムーズ表示あり
# --- .smooth_period: 日数を固定してスムージング
# --- .smooth_message: トレンドの日数をメッセージで表示
subscribers_day_tbl %>%
    plot_time_series(.date_var = optin_time,
                     .value = log(optins + 1),
                     .smooth_period  = "90 days",
                     .smooth_degree  = 1,
                     .smooth_message = TRUE)

# プロット作成
# --- スムーズ表示あり
# --- .smooth_span: 全体の日数からスムージング期間を決定(634 * 0.1419 = 90 : 90/634)
subscribers_day_tbl %>%
    plot_time_series(.date_var = optin_time,
                     .value = log(optins + 1),
                     .smooth_span    = 0.1419,
                     .smooth_degree  = 1,
                     .smooth_message = TRUE)


# * グループごとにスムージング --------------------------------------------

# プロット作成
# --- 予めグループ化しておくとファセットで表示
google_analytics_long_hour_tbl %>%
    group_by(name) %>%
    plot_time_series(date, log(value + 1), .smooth_period = "7 days", .smooth = F)


# プロット作成
# --- .facet_vars引数で指定しても同様の操作が可能
google_analytics_long_hour_tbl %>%
     plot_time_series(date, log(value + 1), .smooth_period = "7 days", .facet_vars = "name")



# * その他の設定 --------------------------------------------------------

# スライダーを追加
subscribers_day_tbl %>%
    plot_time_series(optin_time, optins, .plotly_slider = TRUE)


# インタラクティブなし
subscribers_day_tbl %>%
    plot_time_series(optin_time, optins, .interactive = FALSE)



# 2 ACF診断 -----------------------------------------------------------------------------

# ＜サブタイトル＞
# * ACF / PACF
# * CCF

# * ACF / PACF ------------------------------------------------------------

# ＜ACF＞
# - ACFは原系列とラグ系列の相関係数(自己相関)をラグごとに算出してプロットしたもの
#   --- lags引数で計算するラグ数を指定
#   --- ACFはコレログラムとも呼ばれる
# - 過去と現在の水位がどれくらい関係しているかを示す（過去の記憶の度合い）
#   --- 自己相関の高い系列は、当該系列の過去から将来を予測する余地がある

# ＜PACF＞
# - ACFから時間によって受ける影響を除去した自己相関
#   --- 例えば、T-10はTだけでなくT-1の影響も受けていると考えられる
#   --- PACFはT-1など過去の系列がもたらす影響を排除している


# プロット作成
# --- ACFとPACFの両方が表示される
subscribers_day_tbl %>%
  plot_acf_diagnostics(.date_var = optin_time, .value = log(optins + 1), .lags = 1000)


# 時系列フレーズを使用
# --- 1yearなど
subscribers_day_tbl %>%
  plot_acf_diagnostics(.date_var = optin_time, .value = log(optins + 1), .lags = "1year")


# * CCF -----------------------------------------------------------------

# ＜ポイント＞
# - CCF(相互相関関数)は他の系列とのラグ相関を計測する
# - plot_acf_diagnostics()に複数系列を与えると.指定した系列以外はCCFが表示される
#   --- .value引数で指定した系列はACFとPCF


# データ変換
# --- ワイド型に変換
# --- 日次単位で集計
google_analytics_day_tbl <-
  google_analytics_long_hour_tbl %>%
    pivot_wider(names_from = name, values_from = value) %>%
    summarise_by_time(.date_var = date, .by = "day", across(pageViews:sessions, .fns = sum))

# テーブル結合
subscribers_ga_day_tbl <-
  subscribers_day_tbl %>%
    left_join(google_analytics_day_tbl, by = c("optin_time" = "date")) %>%
    drop_na()

# プロット作成
# --- .value引数で指定した系列はACFとPCFが表示される
# --- それ以外の系列はCCFが表示される
subscribers_ga_day_tbl %>%
  plot_acf_diagnostics(.date_var = optin_time,
                       .value = optins,
                       .ccf_vars = pageViews:sessions,
                       .show_ccf_vars_only = TRUE,
                       .facet_ncol = 1)


# 3 季節性 -----------------------------------------------------------------------------

# ＜ポイント＞
# - 時系列の季節性/周期性を調べるためのプロット


# 季節性プロット
# --- デフォルト表示
# --- 四半期/月/週/日/時間の単位で表示
google_analytics_long_hour_tbl %>%
    group_by(name) %>%
    plot_seasonal_diagnostics(.date_var = date,
                              .value    = log(value + 1))

# 季節性プロット
# --- 特定の時間系列にフォーカス
google_analytics_long_hour_tbl %>%
    group_by(name) %>%
    plot_seasonal_diagnostics(.date_var = date,
                              .value    = log(value + 1),
                              .feature_set = c("hour", "wday.lbl"))

# 季節性プロット
# --- 特定の時間系列にフォーカス
# --- バイオリンプロット
google_analytics_long_hour_tbl %>%
    group_by(name) %>%
    plot_seasonal_diagnostics(.date_var = date,
                              .value    = log(value + 1),
                              .feature_set = c("hour", "wday.lbl"),
                              .geom        = "violin")


# 4 アノマリー --------------------------------------------------------------------

# ＜ポイント＞
# - 時系列におけるイベント検出や異常検知を行う
# - tk_anomaly_diagnostics()をプロット化したもの

# プロット作成
# --- 異常検知の可視化
subscribers_day_tbl %>%
    plot_anomaly_diagnostics(.date_var = optin_time,
                             .value    = optins,
                             .alpha    = 0.01,
                             .max_anomalies = 0.01)

# 参考：データ取得
# --- 異常検知データの取得
subscribers_day_tbl %>%
    tk_anomaly_diagnostics(.date_var = optin_time,
                           .value    = optins,
                           .alpha    = 0.01,
                           .max_anomalies = 0.01)

# プロット作成
# --- グループデータへの適用
google_analytics_long_hour_tbl %>%
    group_by(name) %>%
    plot_anomaly_diagnostics(date, value)

# 参考：データ取得
# --- 異常検知データの取得
google_analytics_long_hour_tbl %>%
  group_by(name) %>%
  tk_anomaly_diagnostics(.date_var = date,
                         .value    = value)


# 5 季節性分解 ---------------------------------------------------------------------

# ＜ポイント＞
# - 時系列から｢Seasonality｣と｢Trend｣を分解して｢Reminder｣を抽出する
# - 季節調整系列を作成する


# プロット作成
# --- 単一系列
subscribers_day_tbl %>%
  plot_stl_diagnostics(.date_var  = optin_time,
                       .value     = log(optins + 1),
                       .frequency = "1 month",
                       .trend     = "1 year")

# プロット作成
# --- グループ単位
google_analytics_long_hour_tbl %>%
    group_by(name) %>%
    plot_stl_diagnostics(date, log(value + 1))




# 6 時系列回帰プロット ---------------------------------------------------------------

# ＜ポイント＞
# - 時系列回帰モデルを作成してプロットする
# - ｢.show_summary = TRUE｣で回帰モデルを確認することができる

# プロット作成
subscribers_day_tbl %>%
  plot_time_series_regression(.date_var = optin_time,
                              .formula = log(optins + 1) ~
                                          as.numeric(optin_time)
                                            + wday(optin_time, label = TRUE)
                                            + month(optin_time, label = TRUE),
                                .show_summary = TRUE)


# プロット作成
google_analytics_long_hour_tbl %>%
    #group_by(name) %>%
    filter(name == "pageViews") %>%
    plot_time_series_regression(.date_var = date,
                                .formula = log(value + 1) ~
                                             as.numeric(date)
                                               + as.factor(hour(date))
                                               + wday(date, label = TRUE)
                                               + month(date, label = TRUE),
                                .show_summary = TRUE)


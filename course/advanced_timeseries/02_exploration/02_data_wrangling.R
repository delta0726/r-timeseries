# ****************************************************************************
# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: TIME SERIES DATA WRANGLING ----
# ****************************************************************************


# ＜目標＞
# - {timetk}の時系列データの操作機能の主要関数をマスターする


# ＜目次＞
# 0 準備
# 1 summarise_by_time()
# 2 pad_by_time()
# 3 filter_by_time()
# 4 mutate_by_time()
# 5 JOINING BY TIME
# 6 日付インデックス
# 7 future_frame()


# 0 準備 -------------------------------------------------------------------------

library(tidyverse)
library(timetk)
library(lubridate)
library(DataExplorer)


# データ1
google_analytics_summary_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")
google_analytics_summary_tbl %>% print()

# データ2
mailchimp_users_tbl <- read_rds("00_data/mailchimp_users.rds")
mailchimp_users_tbl %>% print()

# データ3
transactions_tbl <- read_rds("00_data/transactions_weekly.rds")
transactions_tbl %>% print()



# 1 summarise_by_time()  --------------------------------------------------------

# ＜ポイント＞
# - データフレームの日付インデックスに対して集計を行う
# - dplyrのグループ概念以外に日付の概念を持たせる
#   --- .date_var

# ＜サブタイトル＞
# * 既存データセットと同じ頻度で集計
# * 複数列の同時集計
# * 頻度を指定して集計
# * 日付のラウンド方法


# * 既存データセットと同じ頻度で集計 --------------------------------------

# ＜ポイント＞
# - 単独の集計行を追加する


# データ確認
mailchimp_users_tbl %>% print()
mailchimp_users_tbl %>% glimpse()
mailchimp_users_tbl %>% group_by(member_rating) %>% tally()

# 日次単位で集計
# --- ｢.by｣引数に日付キーワードを入れることで様々な日付単位で集計が可能
# --- グループ化なし（日付でグループ化する必要はない）
# --- 列名を｢optins｣としてカウント
subscribers_daily_tbl <-
  mailchimp_users_tbl %>%
    summarise_by_time(.date_var = optin_time, .by = "day", optins = n())

# 確認
subscribers_daily_tbl %>% print()

# 検証：group_by()で集計
mailchimp_users_tbl %>% group_by(optin_time) %>% tally()

# プロット作成
# --- グループ化あり（日付以外の列を指定）
# --- 行数をカウント
mailchimp_users_tbl %>%
  group_by(member_rating) %>%
  summarise_by_time(.date_var = optin_time, .by = "day", optins = n()) %>%
  plot_time_series(optin_time, optins)


# * 複数列の同時集計 ----------------------------------------------

# ＜ポイント＞
# - across()を用いて複数系列を同時に集計

# 日次単位で集計
# --- 複数列を同時にsum()で集計
google_analytics_summary_daily_tbl <-
  google_analytics_summary_tbl %>%
    mutate(dateHour = ymd_h(dateHour)) %>%
    summarise_by_time(.date_var = dateHour,
                      .by       = "day",
                      across(pageViews:sessions, .fns = sum))

# プロット作成
# --- 確認用
google_analytics_summary_daily_tbl %>%
    pivot_longer(pageViews:sessions) %>%
    plot_time_series(.date_var = dateHour, .value = value, .facet_vars = name)



# * データ頻度を指定して集計 --------------------------------

# ＜ポイント＞
# - 指定した頻度の日付を新たに作成して集計
#   --- ｢weekly｣の場合は元データに含まれる日付の週末が表示される
#   --- 既存のデータセットに入っていない日付(週末)が出てくる


# 週次単位で集計
# --- 指定した頻度の日付を新たに作成して集計
subscribers_weekly_tbl <-
  mailchimp_users_tbl %>%
    summarize_by_time(.date_var = optin_time, .by = "week", optins = n())

# 確認
# --- 既存のデータセットに入っていない日付(週末)が出てくる
subscribers_weekly_tbl %>% print()


# 月次単位で集計
transactions_monthly_tbl <-
  transactions_tbl %>%
    summarise_by_time(.date_var = purchased_at, .by = "1 month", revenue = sum(revenue))

# 確認
# --- 既存のデータセットに入っていない日付(月初日)が出てくる
transactions_monthly_tbl %>% print()


# * 日付のラウンド方法 -----------------------------------

# ＜ポイント＞
# - type引数で集計結果が表示される日付をコントロールする
#   --- デフォルトではFloor(集計単位の初日) / Ceiling(集計単位の最終日の翌日)
#   --- 他に｢Round｣｢Ceiling｣が選択できる


# 元データの確認
transactions_tbl %>% print()

# Floor
# --- 対象期間を月初日付で集計
transactions_tbl %>%
  summarise_by_time(purchased_at,  .by = "1 month",
                    revenue = sum(revenue), .type   = "floor")

# Ceiling
# --- 対象期間を良月初日付で集計
# --- 月末にする場合は日付を%-time%で1日ずらす
transactions_tbl %>%
    summarise_by_time(purchased_at, .by = "1 month",
                      revenue = sum(revenue), .type   = "ceiling") %>%
    mutate(purchased_at = purchased_at %-time% "1 day")

# Round
# --- 対象期間を四捨五入で集計（月次集計の場合は15日）
# --- 表示日付は月初となる
transactions_tbl %>%
    summarise_by_time(purchased_at, .by = "1 month",
                      revenue = sum(revenue), .type   = "round")


# 2 pad_by_time() ----------------------------------------------------------------

# ＜ポイント＞
# - 欠落している日付を補完する

# ＜サブタイトル＞
# * 日次データを補完
# * 週次を日次に変換


# * 日次データを補完 -----------------------------------------------

# データ確認
# --- 2018-06-18から2018-07-03まで欠落している
subscribers_daily_tbl %>% print()


# 日付補完
# --- 開始日も指定できる
# --- 欠落日のデータは.pad_value引数で指定
subscribers_daily_tbl %>%
    pad_by_time(.date_var = optin_time,
                .by = "day",
                .pad_value = 0,
                .start_date = "2018-06-01")


# * 週次を日次に変換 ------------------------------------------------

# データ確認
# --- 週次データ
transactions_tbl %>% print()

# 週次平均の算出
# --- 週次データを日次に変換（欠損日は0）
# --- 週次平均の列を追加
transactions_tbl %>%
    pad_by_time(purchased_at, .by = "day",
                .pad_value = 0, .start_date = "2018-06") %>%
    mutate_by_time(.by = "week", revenue_spread = mean(revenue, na.rm = TRUE))


# 3 filter_by_time() -----------------------------------------------------------

# ＜ポイント＞
# - 指定した日付インデックスでフィルタリング

# ＜サブタイトル＞
# * 開始日指定
# * 期間指定
# * 日付オフセットを活用した期間指定


# * 開始日指定 -------------------------------------------------------

# ＜ポイント＞
# - 欠落している日付を補完する


# データ確認
# --- 2018-11-19に大きなスパイクがある
subscribers_daily_tbl %>%
  plot_time_series(.date_var = optin_time, .value = optins)

# 日付フィルタ
# --- 2018-11-19以降のデータを抽出
subscribers_daily_tbl %>%
  filter_by_time(.start_date = "2018-11-20") %>%
  plot_time_series(optin_time, optins)


# * 期間指定 ----------------------------------------------------------

# 日付フィルタ
# --- 期間指定
subscribers_daily_tbl %>%
  filter_by_time(.start_date = "2019-12", .end_date = "2019") %>%
  plot_time_series(optin_time, optins)


# * 日付オフセットを活用した期間指定 -------------------------------------

# 日付フィルタ
# --- %+time%で期間を指定してフィルタ
subscribers_daily_tbl %>%
  filter_by_time(.start_date = "2019-12",
                 .end_date = "2019-12-01" %+time% "4 weeks") %>%
  plot_time_series(optin_time, optins)



# 4 mutate_by_time() ------------------------------------------------------------

# ＜ポイント＞
# - 日付単位で集計する場合に使用する
# - 日付インデックスでグループ化してmutate()を実行する
#   --- 追加した列は集計関数を伴うのがセオリー
#   --- ローリング計算はslidy()を用いて実現する


# データ確認
# --- 週次データ
transactions_tbl %>% print()

# 集計列の追加
# --- ｢purchased_at｣を3か月ごとに集計
transactions_tbl %>%
  mutate_by_time(.date_var       = purchased_at,
                 .by             = "3 month",
                 revenue_mean    = mean(revenue),
                 revenue_median  = median(revenue),
                 revenue_max     = max(revenue),
                 revenue_min     = min(revenue)) %>%
  pivot_longer(contains("revenue")) %>%
  plot_time_series(purchased_at, value, name, .smooth = FALSE)


# 5 JOINING BY TIME  ------------------------------------------------------------

# ＜ポイント＞
# - 開始日や頻度の異なるデータセットを結合する
#   --- pad_by_time()で日付を生成してから結合する
#   --- 結合自体は{dplyr}のjoin()系の関数を使用する

# ＜サブタイトル＞
# * 日付データで結合
# * 結合データの検証


# * 日付データで結合 -------------------------------------------------

# データ確認
subscribers_daily_tbl %>% print()
google_analytics_summary_daily_tbl %>% print()

# 列の結合
# --- 欠損日付の補完
subscribers_google_joined_tbl <-
  subscribers_daily_tbl %>%
    pad_by_time(.pad_value = 0, .start_date = "2018-06") %>%
    left_join(google_analytics_summary_daily_tbl, by = c("optin_time" = "dateHour"))

# 確認
subscribers_google_joined_tbl %>% print()


# * 結合データの検証 -------------------------------------------------

# 欠損値確認
# --- 50％程度が欠損している
subscribers_google_joined_tbl %>% plot_missing()

# 日付インデックスの確認
# --- データ開始日が異なることを確認
subscribers_google_joined_tbl %>% tk_summary_diagnostics()
google_analytics_summary_daily_tbl %>% tk_summary_diagnostics()

# プロット作成
subscribers_google_joined_tbl %>%
    pivot_longer(-optin_time) %>%
    plot_time_series(optin_time, value, .color_var = name)


# * Visualization Techniques (Relationships) ------------------------

# データ加工
# --- 対数変換
# --- 基準化
log_standardized_subscribers_joined_tbl <-
  subscribers_google_joined_tbl %>%
    drop_na() %>%
    mutate(across(optins:sessions, .fns = log1p)) %>%
    mutate(across(optins:sessions, .fns = standardize_vec)) 

# 確認
log_standardized_subscribers_joined_tbl %>% print()

# プロット作成
log_standardized_subscribers_joined_tbl %>%
  pivot_longer(optins:sessions) %>%
  group_by(name) %>%
  plot_time_series(optin_time, value, name, .smooth = FALSE)


# プロット作成
# --- optinsとの関係性を確認
# --- CCFで見ると理解しやすい
log_standardized_subscribers_joined_tbl %>%
    plot_acf_diagnostics(optin_time, optins,
                         .ccf_vars = pageViews:sessions,
                         .show_ccf_vars_only = TRUE)



# 6 日付インデックス  ------------------------------------------------------------

# ＜ポイント＞
# - 日付インデックスや日付操作を確認する

# ＜サブタイトル＞
# * インデックスの抽出
# * 日付インデックスの作成
# * 休日カレンダー
# * 日付のオフセット計算
# * 将来の日付ベクトルの作成


# * インデックスの抽出 -----------------------------------

# 日付インデックスの抽出
subscribers_daily_tbl %>% tk_index() %>% str()


# * 日付インデックスの作成 ----------------------------------------

# ＜ポイント＞
# - データフレームの日付データは｢日付インデックス｣として扱われる
# - {timetk}では日付ベクトルの作成もサポートしている

# 数値ベクトル
values <- 1:100

# 日付ベクトルの作成
tk_make_timeseries("2011", by = "month", length_out = 100)

# データ作成
tk_index_tbl <-
  tibble(date = tk_make_timeseries("2011", by = "month", length_out = 100),
         values)

# 確認
tk_index_tbl %>% print()

# プロット作成
tk_index_tbl %>% plot_time_series(date, values, .smooth = FALSE)


# * 休日カレンダー ----------------------------------------------------

# ＜ポイント＞
# - {timeDate}のカレンダーをラップしている


# カレンダーの取得
tk_make_holiday_sequence("2011", "2021", calendar = "NYSE")

# 日付ベクトルにカレンダーを追加
# --- NYSEの休日を日付列とする
# --- 他のカレンダーを取得
tk_make_holiday_sequence("2011", "2021", calendar = "NYSE") %>%
  tk_get_holiday_signature() %>%
  glimpse()

# 日付ベクトルにカレンダーを追加
tk_make_timeseries("2011") %>%
    tk_get_holiday_signature()


# * 日付のオフセット計算 ----------------------------------------------------

# ＜ポイント＞
# - {timetk}では日付計算の演算子が用意されている


# 演算子による日付計算
"2011-01-01" %+time% "1 day"
"2011-01-01" %-time% "1 day"
"2011-01-01" %+time% "1 year"

# 日付ベクトルに適用
# --- 1年後
tk_make_timeseries("2011") %+time% "1 year"


# * 将来の日付ベクトルの作成 ------------------------------------------------

# ＜ポイント＞
# - 過去の日付ベクトルを元に将来の日付ベクトルを作成

# 日付ベクトルの作成
# --- 指定した日付ベクトルに続く日付を作成
tk_make_timeseries("2011-01") %>%
  tk_make_future_timeseries(length_out = 30)


# 日付ベクトルの作成
# --- 前後で日付の頻度を変えることが可能
tk_make_timeseries("2011", by = "quarter") %>%
  tk_make_future_timeseries(length_out = "1 year")


# 7 future_frame()  ------------------------------------------------------------

# ＜ポイント＞
# - 過去のデータフレームを元に将来日付のデータフレームを作成

# ＜サブタイトル＞
# * Future Frame
# * モデリング
# * 可視化


# * Future Frame ------------------------------------------------

# データ末尾を確認
google_analytics_summary_daily_tbl %>% tail()

# 将来日付のデータフレーム
# ---元の日付と結合されるわけではない
google_analytics_summary_daily_tbl %>%
    future_frame(.length_out = 30)


# * モデリング ----------------------------------------------------

# ＜ポイント＞
# - 将来の日付フレームをモデリングで活用
# - このフローを応用して将来の予測データを作成していく

# モデル構築
# --- 時系列の線形回帰
# --- 日付を説明変数としている
model_fit_lm <-
   lm(pageViews ~ as.numeric(dateHour) + wday(dateHour, label = TRUE),
      data = google_analytics_summary_daily_tbl)

# 確認
model_fit_lm %>% print()

# 将来日付のデータフレーム作成
future_tbl <-
  google_analytics_summary_daily_tbl %>%
    future_frame(.length_out = "2 months")

# 予測データの作成
# --- 将来日付を説明変数として予測
predictions_vec <-
  model_fit_lm %>%
    predict(newdata = future_tbl) %>%
    as.vector()


# * 可視化 --------------------------------------------------------

# 過去データ
df_actual <-
  google_analytics_summary_daily_tbl %>%
    select(dateHour, pageViews) %>%
    add_column(type = "actual")


# 予測データ
df_predict <-
  future_tbl %>%
    mutate(pageViews = predictions_vec,
           type      = "prediction")


# プロット作成
df_actual %>%
  bind_rows(df_predict) %>%
    plot_time_series(.date_var = dateHour,
                     .value = pageViews, type, .smooth = FALSE)


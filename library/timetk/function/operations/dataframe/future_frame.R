# Title     : future_frame
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/5
# URL       : https://business-science.github.io/timetk/reference/future_frame.html


# ＜ポイント＞
# - 現在のデータフレームの日付インデックスに基づいて、将来分のデータフレームを作成


# ＜構文＞
# future_frame(
#  .data,
#  .date_var,
#  .length_out,
#  .inspect_weekdays = FALSE,
#  .inspect_months = FALSE,
#  .skip_values = NULL,
#  .insert_values = NULL
#)


# ＜引数＞
# - length_out       : 期間数の指定 (キーワードor数値)
# - .inspect_weekdays: 土日を除外するかを指定
# - skip_values      : 除外する日付をベクトルで指定（休日など）



# 1.準備 --------------------------------------------------------------

library(dplyr)
library(tidyquant)
library(timetk)



# 2.1系列の時系列データ ---------------------------------------------------

# データ確認
taylor_30_min %>% print()
taylor_30_min %>% glimpse()


# 日付サマリー
# --- 8/28で終了している
taylor_30_min %>% tk_index() %>% tk_get_timeseries_summary()


# 将来のデータフレーム作成
# --- 日付インデックスの列のみのデータフレーム
# --- 30-min interval data
taylor_30_min_future <- taylor_30_min %>% future_frame(date, .length_out = "1 week")
taylor_30_min_future %>% print()


# 日付サマリー
# --- 1週間分のデータであることを確認
taylor_30_min_future %>% tk_index() %>% tk_get_timeseries_summary()



# 3.複数系列の時系列データ ---------------------------------------------------

# 関数定義
# --- 期間チェック用関数
check_term <- function(df){
  df %>%
    group_by(id) %>%
    summarise(start_date = min(date),
              final_date = max(date))
}


# データ確認
m4_daily %>% print()
m4_daily %>% glimpse()
m4_daily %>% group_by(id) %>% tally()


# グループごとの開始日と最終日
# --- グループごとに異なる
m4_daily %>% check_term()


# 日付サマリー
# --- データフレームの全期間で作成される
m4_daily %>% tk_index() %>% tk_get_timeseries_summary()


# 将来のデータフレーム作成
m4_daily_future <-
  m4_daily %>%
    group_by(id) %>%
    future_frame(date, .length_out = "6 weeks")


# 確認
# --- それぞれの系列ごとに6週間分作成されている
m4_daily_future %>% print()
m4_daily_future %>% check_term()



# 4.期間数を指定して作成 ---------------------------------------------------

# 将来のデータフレーム作成
m4_daily_future <-
  m4_daily %>%
    group_by(id) %>%
    future_frame(date, .length_out = 100)


# 確認
# --- それぞれの系列ごとに6週間分作成されている
m4_daily_future %>% print()
m4_daily_future %>% check_term()



# 5.休日を考慮して作成 ---------------------------------------------------


# Remove Non-Working Days (Weekends & Holidays)
holidays <-
  tk_make_holiday_sequence(start_date = "2017-01-01",
                           end_date   = "2017-12-31",
                           calendar   = "NYSE")

# 確認
holidays %>% print()

FANG %>%
  group_by(symbol) %>%
  future_frame(.length_out       = "1 year",
               .inspect_weekdays = TRUE,
               .skip_values      = holidays)

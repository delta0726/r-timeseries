# Title     : filter_by_time
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/5
# URL       : https://business-science.github.io/timetk/reference/filter_by_time.html


# ＜ポイント＞
# - データフレームにおいて時間でフィルタリングを行う
#   --- tibbletime::filter_time()を元にしている
#   --- 日付の簡易表現やキーワードを使った指定が可能


# ＜構文＞
# filter_by_time(.data, .date_var, .start_date = "start", .end_date = "end")


# ＜日付指定：簡易表現＞
# - year     : start_date = '2013'  end_date = '2015'
# - month    : start_date = '2013-01'  end_date = '2015-01'
# - day      : start_date = '2013-01-31'  end_date = '2015-01-31'
# - variation: start_date = '2013'  end_date = '2015-01-31'

# ＜日付指定：キーワード＞
# - start : 最も小さい日付から
# - end   : 最も大きい日付まで



# 1.使用例 --------------------------------------------------------------

library(tidyverse)
library(tidyquant)
library(timetk)


# データ確認
# --- 通常のデータフレーム
# --- 日付列はdate型
FANG %>% print()
FANG %>% glimpse()


# 時系列プロット作成
# --- 時間でフィルタ
FANG %>%
  group_by(symbol) %>%
  filter_by_time(.start_date = "start", .end_date = "2013-02") %>%
  plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE)

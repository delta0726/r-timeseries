# Title     : tk_summary_diagnostics
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/6
# URL       : https://business-science.github.io/timetk/reference/tk_summary_diagnostics.html



# ＜ポイント＞
# - データフレームに含まれる日付列のサマリーを返す
# - 日付列を指定しない場合、最初の日付列を自動検知で見つけてくれる（列をメッセージ表示）
#   --- 明示的に指定するとメッセージが表示されない
# - tk_index()で日付インデックスを取得した後に、tk_get_timeseries_summary()を適用してもよい


# ＜構文＞
# - tk_summary_diagnostics(.data, .date_var)



# 1.使用例 ------------------------------------------------------------------------


library(tidyverse)
library(timetk)


# データ確認
m4_monthly %>% print()
m4_monthly %>% glimpse()


# グループ数の確認
m4_monthly %>% group_by(id) %>% tally()


# 日付サマリーの取得
# --- 単一系列
m4_monthly %>%
  filter(id == "M750") %>%
  tk_summary_diagnostics()


# 日付サマリーの取得
# --- 単一系列
# --- 日付列を指定
m4_monthly %>%
  filter(id == "M750") %>%
  tk_summary_diagnostics(.date = date)


# 日付サマリーの取得
# --- 複数系列
m4_monthly %>%
  group_by(id) %>%
  tk_summary_diagnostics()
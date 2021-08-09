# ***************************************************************************************
# Library   : timetk
# Function  : tk_summary_diagnostics
# Created on: 2021/8/10
# URL       : https://business-science.github.io/timetk/reference/tk_summary_diagnostics.html
# ***************************************************************************************


# ＜ポイント＞
# - データフレームに含まれる日付列のサマリーを返す


# ＜構文＞
# - tk_summary_diagnostics(.data, .date_var)


# ＜使用例＞
# 0 準備
# 1 単一系列の日付サマリー
# 2 複数系列の日付サマリー


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)


# データ確認
m4_monthly %>% print()
m4_monthly %>% group_by(id) %>% tally()


# 1 単一系列の日付サマリー -------------------------------------------------------

# 日付サマリー
# --- 単一系列をフィルタ
m4_monthly %>%
  filter(id == "M750") %>%
  tk_summary_diagnostics(.date = date)


# 2 複数系列の日付サマリー -------------------------------------------------------

# 日付サマリー
# --- 複数系列をグループ化
m4_monthly %>%
  group_by(id) %>%
  tk_summary_diagnostics(.date = date)

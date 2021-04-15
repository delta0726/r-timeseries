# Title     : tk_acf_diagnostics
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/6
# URL       : https://business-science.github.io/timetk/reference/tk_acf_diagnostics.html



# ＜ポイント＞
# - データフレームに含まれる日付列のサマリーを返す
# - 日付列は最初の日付列を自動検知で見つけてくれる（列をメッセージ表示）
#   --- 明示的に指定するとメッセージが表示されない


# ＜構文＞
# - tk_acf_diagnostics(.data, .date_var, .value, .ccf_vars = NULL, .lags = 1000)



# 1.使用例 ------------------------------------------------------------------------


library(tidyverse)
library(tidyquant)
library(timetk)


# データ確認
m4_monthly %>% print()
m4_monthly %>% glimpse()


# ACF分解
# --- 単一系列
FANG %>%
  filter(symbol == "FB") %>%
  tk_acf_diagnostics(date, adjusted,                # ACF & PACF
                     .ccf_vars = c(volume, close),  # CCFs
                     .lags     = 500)


# ACF分解
# --- 複数系列
FANG %>%
  group_by(symbol) %>%
  tk_acf_diagnostics(date, adjusted,
                     .ccf_vars = c(volume, close),
                     .lags     = "3 months")


# ACF分解
# --- 複数系列
FANG %>%
  group_by(symbol) %>%
  tk_acf_diagnostics(
      date, diff_vec(adjusted),  # Apply differencing transformation
      .lags = 0:500
  )
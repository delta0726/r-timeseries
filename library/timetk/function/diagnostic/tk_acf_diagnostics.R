# ***************************************************************************************
# Library   : timetk
# Function  : tk_acf_diagnostics
# Created on: 2021/8/8
# URL       : https://business-science.github.io/timetk/reference/tk_acf_diagnostics.html
# ***************************************************************************************


# ＜ポイント＞
# - 時系列データから自己相関(ACF)/偏自己相関(PACF)/相互相関(CCF)を計算する
#   --- 全ての結果が1つのデータフレームに表示される
 

# ＜構文＞
# - tk_acf_diagnostics(.data, .date_var, .value, .ccf_vars = NULL, .lags = 1000)


# ＜目次＞
# 0 準備
# 1 単一系列の計算
# 2 複数系列の計算
# 3 差分系列における計算


# 0 準備 ------------------------------------------------------------------------


library(tidyverse)
library(tidyquant)
library(timetk)


# データ確認
# --- 4系列を含むデータセット
FANG %>% print()
FANG %>% group_by(symbol) %>% tally()


# 1 単一系列の計算 -------------------------------------------------------------

# 自己相関等の計算
# --- 単一系列のみフィルタ
FANG %>%
  filter(symbol == "FB") %>%
  tk_acf_diagnostics(date, adjusted, 
                     .ccf_vars = c(volume, close),　
                     .lags     = 500)

# プロット確認
FANG %>%
  filter(symbol == "FB") %>%
  plot_acf_diagnostics(date, adjusted, 
                       .ccf_vars = c(volume, close),　
                       .lags     = 500)


# 2 複数系列の計算 -------------------------------------------------------------

# 自己相関等の計算
# --- 複数系列をグループ化
FANG %>%
  group_by(symbol) %>%
  tk_acf_diagnostics(date, adjusted,
                     .ccf_vars = c(volume, close),
                     .lags     = "3 months")

# プロット確認
FANG %>%
  group_by(symbol) %>%
  plot_acf_diagnostics(date, adjusted, 
                       .ccf_vars = c(volume, close),　
                       .lags     = 500)


# 3 差分系列における計算 ---------------------------------------------------------

# ＜ポイント＞
# - 検査する系列を差分系列に変換してから分析する


# 自己相関等の計算
# --- 差分系列の計算
# --- 複数系列をグループ化
FANG %>%
  group_by(symbol) %>%
  tk_acf_diagnostics(date, 
                     diff_vec(adjusted), 
                     .lags = 0:500)

# プロット確認
# --- 自己相関はほとんど消えている
FANG %>%
  group_by(symbol) %>%
  plot_acf_diagnostics(date, 
                       diff_vec(adjusted), 
                       .lags = 0:500)

# ***************************************************************************************
# Library   : timetk
# Function  : tk_get_trend
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/tk_get_frequency.html
# ***************************************************************************************


# ＜概要＞
# - 日付インデックスから周期性分析で使用するトレンドの長さを取得する
#   --- デフォルトのトレンド期間はテンプレートで管理されている


# ＜構文＞
# tk_get_trend(idx, period = "auto", message = TRUE)


# ＜目次＞
# 0 準備
# 1 トレンドの取得


# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(timetk)


# データ準備
# --- 日付ベクトルの取得
idx_FB <- FANG %>% filter(symbol == "FB") %>% pull(date)
idx_FB %>% print()

# 時間テンプレートの確認
# --- デフォルトのトレンド期間はテンプレートで管理されている
get_tk_time_scale_template()


# 1 トレンドの取得 --------------------------------------------------------------

# トレンドの取得
# --- テンプレートから取得
idx_FB %>% tk_get_trend(period = "auto")

# トレンドの取得
# --- 明示的に指定
idx_FB %>% tk_get_trend(period = "1 year")

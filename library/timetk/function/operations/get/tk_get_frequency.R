# ***************************************************************************************
# Library   : timetk
# Function  : tk_get_frequency
# Created on: 2021/8/14
# URL       : https://business-science.github.io/timetk/reference/tk_get_frequency.html
# ***************************************************************************************


# ＜概要＞
# - 日付インデックスの頻度に対する1つ上の頻度を取得する
#   --- 日次の場合は週次
#   --- 周期性分析で使用する


# ＜構文＞
# tk_get_frequency(idx, period = "auto", message = TRUE)


# ＜目次＞
# 0 準備
# 1 頻度の取得


# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(timetk)


# データ準備
# --- 日付ベクトルの取得
idx_FB <- FANG %>% filter(symbol == "FB") %>% pull(date)
idx_FB

# 時間テンプレートの確認
# --- デフォルトのfrequency期間はテンプレートで管理されている
# --- 元データに対して1つ上の単位が適用される
get_tk_time_scale_template()


# 1 頻度の取得 -------------------------------------------------------------------

# 頻度の取得
# --- テンプレートから取得
idx_FB %>% tk_get_frequency(period = "auto")

# 頻度の取得
# --- テンプレートから取得
idx_FB %>% tk_get_frequency(period = "month")

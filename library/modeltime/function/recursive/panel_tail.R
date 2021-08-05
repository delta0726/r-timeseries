# ***************************************************************************************
# Library   : modeltime
# Function  : panel_tail
# Created on: 2021/8/6
# URL       : https://business-science.github.io/modeltime/reference/panel_tail.html
# ***************************************************************************************


# ＜概要＞
# - パネル(グループ)ごとの末尾データを取得する（dplyr::group_by()を適用する必要はない）
# - 再帰的処理をする場合のチェック用に使用


# ＜構文＞
# panel_tail(data, id, n)


# ＜使用例＞

# ライブラリ
library(tidyverse)
library(timetk)
library(modeltime)

# データ確認
m4_monthly %>% group_by(id) %>% tally()
m4_monthly %>% group_split(id) %>% map(tk_summary_diagnostics) %>% map(select, 1:4)

# データ末尾の取得
m4_monthly %>%
  panel_tail(id = id, n = 3) %>%
  print(n = nrow(.))

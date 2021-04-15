# Title     : tk_seasonal_diagnostics
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/6
# URL       : https://business-science.github.io/timetk/reference/tk_summary_diagnostics.html



# ＜ポイント＞
# - データフレームに含まれる日付列をyear/monthなどの要素に分解して列を追加する
# - 元の日付列の単位よりも大きい要素が表示される
#   --- 日次データの場合 : month / quarter / year
#   --- .feature_set引数で表示するものを指定することができる


# ＜構文＞
# -tk_seasonal_diagnostics(.data, .date_var, .value, .feature_set = "auto")



# 1.準備 ------------------------------------------------------------------------


library(tidyverse)
library(timetk)



# 2.時間データの分解 ----------------------------------------------------------------

# データ確認
m4_hourly %>% print()
m4_hourly %>% glimpse()


# 時間列の分解
# --- 時間データ
m4_hourly %>%
  group_by(id) %>%
  tk_seasonal_diagnostics(.date_var = date, .value = value)



# 2.日次データの分解 ----------------------------------------------------------------

# データ確認
m4_monthly %>% print()
m4_monthly %>% glimpse()


# Monthly Data
m4_monthly %>%
  group_by(id) %>%
  tk_seasonal_diagnostics(.date_var = date, .value = value)



# 3.週次データの分解 ----------------------------------------------------------------

# データ確認
m4_monthly %>% print()
m4_monthly %>% glimpse()


# 分解
m4_weekly %>%
  group_by(id) %>%
  tk_seasonal_diagnostics(date, log(value))



# 4.元データより細かい単位を表示 -----------------------------------------------------

m4_hourly %>%
    group_by(id) %>%
    tk_seasonal_diagnostics(date, value, .feature_set = c("hour", "week"))
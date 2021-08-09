# ***************************************************************************************
# Library   : timetk
# Function  : tk_seasonal_diagnostics
# Created on: 2021/8/10
# URL       : https://business-science.github.io/timetk/reference/tk_seasonal_diagnostics.html
# ***************************************************************************************


# ＜ポイント＞
# - 日付インデックスから日付要素を生成して列に追加する
# - 元の日付列の単位よりも大きい要素が表示される
#   --- 日次データの場合 : month / quarter / year
#   --- .feature_set引数で表示するものを指定することができる


# ＜構文＞
# -tk_seasonal_diagnostics(.data, .date_var, .value, .feature_set = "auto")


# ＜目次＞
# 0 準備
# 1 時間データの日付要素の分解
# 2 日次データの分解
# 3 週次データの分解
# 4 出力単位を指定して表示


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(timetk)


# ローケール設定変更
Sys.setlocale("LC_TIME", "English")
Sys.getlocale("LC_TIME")



# 1 時間データの日付要素の分解 -------------------------------------------------

# データ確認
m4_hourly %>% print()
m4_hourly %>% group_by(id) %>% tally()


# 日付要素の追加
# --- hour / wday.lbl / week
m4_hourly %>%
  group_by(id) %>%
  tk_seasonal_diagnostics(.date_var = date, 
                          .value    = value)

# プロット作成
m4_hourly %>%
  plot_seasonal_diagnostics(.date_var    = date, 
                            .value       = value, 
                            .interactive = FALSE)


# 2 日次データの分解 ----------------------------------------------------------------

# データ確認
m4_monthly %>% print()
m4_monthly %>% group_by(id) %>% tally()

# 日付要素の追加
# --- month.lbl / quarter / year 
m4_monthly %>%
  group_by(id) %>%
  tk_seasonal_diagnostics(.date_var = date, .value = value)

# プロット作成
m4_monthly %>%
  group_by(id) %>%
  plot_seasonal_diagnostics(.date_var    = date, 
                            .value       = value, 
                            .interactive = FALSE)



# 3 週次データの分解 ----------------------------------------------------------------

# データ確認
m4_weekly %>% print()
m4_weekly %>% group_by(id) %>% tally()

# 日付要素の追加
# --- month.lbl / quarter / year 
m4_weekly %>%
  group_by(id) %>%
  tk_seasonal_diagnostics(date, log(value))

# プロット作成
m4_weekly %>%
  group_by(id) %>%
  plot_seasonal_diagnostics(.date_var    = date, 
                            .value       = value, 
                            .interactive = FALSE)


# 4 出力単位を指定して表示 --------------------------------------------------------

# データ確認
m4_hourly %>% print()
m4_hourly %>% group_by(id) %>% tally()

# 日付要素の追加
m4_hourly %>%
  group_by(id) %>%
  tk_seasonal_diagnostics(date, value, .feature_set = c("hour", "week"))

# プロット作成
m4_hourly %>%
  group_by(id) %>%
  plot_seasonal_diagnostics(.date_var    = date, 
                            .value       = value, 
                            .feature_set = c("hour", "week"), 
                            .interactive = FALSE)

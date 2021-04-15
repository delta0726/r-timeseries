# Title     : tk_augment_timeseries_signature
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/4
# URL       : https://business-science.github.io/timetk/reference/tk_augment_timeseries.html



# ＜ポイント＞
# - 日付データを持つデータフレームに日付属性を追加する




# ＜構文＞
# tk_augment_timeseries_signature(.data, .date_var = NULL)

# .data     ：A time-based tibble / time-series object
# .date_var ：tibbleの場合に日付列を指定



# 1.準備 -----------------------------------------------------------------------

library(dplyr)
library(timetk)


# データ確認
m4_daily %>% print()
m4_daily %>% glimpse()


# 件数確認
# --- グループごと
m4_daily %>% group_by(id) %>% tally()



# 2.日付属性を追加 ----------------------------------------------------------------

# データフレームに日付属性を追加
m4_daily %>%
  group_by(id) %>%
  tk_augment_timeseries_signature(.date_var = date)


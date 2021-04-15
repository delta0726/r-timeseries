# Title     : tk_get_holiday_signature
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/5
# URL       : https://business-science.github.io/timetk/reference/tk_get_holiday.html



# ＜ポイント＞
# - データフレームにTimeseries Signatureを追加する
#   --- tk_get_timeseries_signature()は日付インデックスを引数とする点で異なる


# ＜構文＞
# - tk_get_holiday_signature(
#  idx,
#  holiday_pattern = ".",
#  locale_set = c("all", "none", "World", "US", "CA", "GB", "FR", "IT", "JP", "CH",
#    "DE"),
#  exchange_set = c("all", "none", "NYSE", "LONDON", "NERC", "TSX", "ZURICH")
# )
#
# tk_get_holidays_by_year(years = year(today()))




# 1.使用例 ----------------------------------------------------------

library(tidyverse)
library(timetk)


# データ作成
# --- 日付のみ
dates_in_2017_tbl <-
  tibble(index = tk_make_timeseries(start_date = "2017-01-01",
                                    end_date = "2017-12-31",
                                    by = "day"))


# 確認
dates_in_2017_tbl %>% print()
dates_in_2017_tbl %>% tk_index() %>% tk_get_timeseries_summary()



# 2.休日フラグの追加 ----------------------------------------------------------

#
dates_in_2017_tbl %>%
    tk_augment_holiday_signature(
        index,
        .holiday_pattern = "^$",   # Returns nothing on purpose
        .locale_set      = "none",
        .exchange_set    = "NYSE")


dates_in_2017_tbl %>%
    tk_augment_holiday_signature(
        index,
        .holiday_pattern = "US_",
        .locale_set      = "US",
        .exchange_set    = "none")


dates_in_2017_tbl %>%
    tk_augment_holiday_signature(
        index,
        .holiday_pattern = "(World)|(IT_)",
        .locale_set      = c("World", "IT"),
        .exchange_set    = "none")



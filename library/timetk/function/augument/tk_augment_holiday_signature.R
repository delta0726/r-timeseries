# ***************************************************************************************
# Library   : timetk
# Function  : tk_augment_holiday_signature
# Created on: 2021/8/8
# URL       : https://business-science.github.io/timetk/reference/tk_augment_holiday.html
# ***************************************************************************************


# ＜ポイント＞
# - データフレームにTimeseries Signatureを追加する
#   --- tk_get_timeseries_signature()は日付インデックスを引数とする点で異なる


# ＜構文＞
# tk_augment_holiday_signature(
#   .data,
#   .date_var = NULL,
#   .holiday_pattern = ".",
#   .locale_set = c("all", "none", "World", "US", "CA", "GB", "FR", "IT", "JP", "CH",
#     "DE"),
#   .exchange_set = c("all", "none", "NYSE", "LONDON", "NERC", "TSX", "ZURICH")
# )


# ＜使用例＞
# 0 準備
# 2.休日フラグの追加


# 0 準備 -------------------------------------------------------------------------------

# ライブラリ
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
dates_in_2017_tbl %>% tk_index() %>% tk_get_timeseries_summary() %>% select(1:4)



# 2 休日フラグの追加 ----------------------------------------------------------

#
dates_in_2017_tbl %>%
    tk_augment_holiday_signature(.date_var        = index,
                                 .holiday_pattern = "^$",
                                 .locale_set      = "none",
                                 .exchange_set    = "NYSE")

dates_in_2017_tbl %>%
    tk_augment_holiday_signature(.date_var        = index,
                                 .holiday_pattern = "US_",
                                 .locale_set      = "US",
                                 .exchange_set    = "none")


dates_in_2017_tbl %>%
  tk_augment_holiday_signature(.date_var        = index,
                               .holiday_pattern = "(World)|(IT_)",
                               .locale_set      = c("World", "IT"),
                               .exchange_set    = "none")



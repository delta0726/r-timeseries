# Title     : time_series_split()
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/9
# URL       : https://business-science.github.io/timetk/reference/time_series_split.html



# ＜ポイント＞
# - 時系列方向にデータセットを訓練データ/テストデータに分割する



# ＜構文＞
# time_series_split(
#   data,
#   date_var = NULL,
#   initial = 5,
#   assess = 1,
#   skip = 1,
#   lag = 0,
#   cumulative = FALSE,
#   slice = 1,
#   ...
# )


# ＜引数＞




# 1.準備 ----------------------------------------------------------------------------


library(tidyverse)
library(timetk)


# データ準備
m750 <- m4_monthly %>% filter(id == "M750")


# 確認
m750 %>% print()
m750 %>% glimpse()
m750 %>% tk_summary_diagnostics()



m750 %>%
    time_series_split(initial = "10 years", assess = "3 years")



m750 %>%
    time_series_split(
        initial = "10 years",
        assess  = "3 years",
        skip    = "3 years",
        slice   = 2          # <- Returns 2nd slice, 3-years back
    )


m750 %>%
    time_series_split(
        initial = "10 years",
        assess  = "3 years",
        skip    = "3 years",
        slice   = 2,
        lag     = "1 year"   # <- Overlaps training/testing by 1 year
    )

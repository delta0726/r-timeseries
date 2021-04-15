# Title     : tk_get_frequency
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/23
# URL       : https://business-science.github.io/timetk/reference/tk_get_frequency.html



# ＜ポイント＞





# ＜構文＞
# tk_get_frequency(idx, period = "auto", message = TRUE)




# 1.準備 -----------------------------------------------------------------------


library(tidyverse)
library(tidyquant)
library(timetk)


# データ準備
data("FANG")


# データ確認
FANG %>% print()
FANG %>% glimpse()


# デイリーデータ
# --- FBのみ抽出
idx_FB <- FANG %>% filter(symbol == "FB") %>% pull(date)
idx_FB %>% print()



# 2.使用例 ----------------------------------------------------------------


# Automated Frequency Calculation
idx_FB %>% tk_get_frequency(period = "auto")


# Automated Trend Calculation
idx_FB %>% tk_get_trend(period = "auto")


# Manually Override Trend
idx_FB %>% tk_get_trend(period = "1 year")



# 時間テンプレートの確認
get_tk_time_scale_template()
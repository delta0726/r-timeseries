# ***************************************************************************************
# Library   : timetk
# Function  : step_holiday_signature
# Created on: 2021/8/15
# URL       : https://business-science.github.io/timetk/reference/step_holiday_signature.html
# ***************************************************************************************


# ＜概要＞
# - 日付に対する休日テーブルを追加するレシピステップ


# ＜構文＞
# step_holiday_signature(
#   recipe,
#   ...,
#   holiday_pattern = ".",
#   locale_set = "all",
#   exchange_set = "all",
#   role = "predictor",
#   trained = FALSE,
#   columns = NULL,
#   features = NULL,
#   skip = FALSE,
#   id = rand_id("holiday_signature")
# )


# ＜目次＞
# 0 準備
# 1 レシピの作成
# 2 レシピサマリーの確認


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(recipes)
library(timetk)
library(tidyverse)

# データ準備
dates_in_2017_tbl <-
  tibble(index = tk_make_timeseries("2017-01-01", "2017-12-31", by = "day"))

# データ確認
dates_in_2017_tbl %>% print()
dates_in_2017_tbl %>% glimpse()


# 1 レシピの作成 -------------------------------------------------------------

# レシピ定義
# --- カレンダーを追加
rec_holiday <-
  recipe(~ ., dates_in_2017_tbl) %>%
    step_holiday_signature(index,
                           holiday_pattern = "^US_",
                           locale_set      = "US",
                           exchange_set    = "NYSE")

# 確認
rec_holiday %>% prep() %>% juice()
rec_holiday %>% prep() %>% juice() %>% glimpse()


# 2 レシピサマリーの確認 ---------------------------------------------------------

# サマリー
# --- 追加したカレンダーが系列として表示される
rec_holiday %>% summary()
rec_holiday %>% prep() %>% summary()

# 処理内容の表示
# --- トップレベル
# --- 詳細レベル（カレンダー名が表示される）
rec_holiday %>% prep() %>% tidy()
rec_holiday %>% prep() %>% tidy(1)

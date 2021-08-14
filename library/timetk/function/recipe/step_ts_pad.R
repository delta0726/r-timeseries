# ***************************************************************************************
# Library   : timetk
# Function  : step_ts_pad
# Created on: 2021/8/15
# URL       : https://business-science.github.io/timetk/reference/step_ts_pad.html
# ***************************************************************************************


# ＜概要＞
# - 指定された時間間隔で欠損した日付を補完するレシピステップ
#   --- recipes::pad_by_time()をレシピ化したもの


# ＜構文＞
# step_ts_pad(
#   recipe,
#   ...,
#   by = "day",
#   pad_value = NA,
#   role = "predictor",
#   trained = FALSE,
#   columns = NULL,
#   skip = FALSE,
#   id = rand_id("ts_padding")
# )


# ＜目次＞
# 0 準備
# 1 レシピの作成
# 2 レシピサマリーの確認


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(recipes)
library(timetk)

# データ準備
FB_tbl <-
  FANG %>%
    filter(symbol == "FB") %>%
    select(symbol, date, adjusted)

# データ確認
# --- 土日の日付が含まれないことを確認
FB_tbl %>% print()
FB_tbl %>% glimpse()


# 1 レシピの作成 -------------------------------------------------------------

# レシピ定義
# --- 土日のレコードを追加
recipe_ts_pad <-
  recipe(~ ., data = FB_tbl) %>%
    step_ts_pad(date, by = "day", pad_value = NA)

# 確認
# --- 土日のレコードを追加されている
recipe_ts_pad %>% prep() %>% bake(new_data = FB_tbl)
recipe_ts_pad %>% prep() %>% bake(new_data = FB_tbl) %>% glimpse()


# 2 レシピサマリーの確認 ---------------------------------------------------------

# サマリー
# --- 指定系列を変換しているので同じ結果
recipe_ts_pad %>% summary()
recipe_ts_pad %>% prep() %>% summary()

# 処理内容の表示
# --- トップレベル
# --- 詳細レベル（最適ラムダが表示される）
recipe_ts_pad %>% prep() %>% tidy()
recipe_ts_pad %>% prep() %>% tidy(1)

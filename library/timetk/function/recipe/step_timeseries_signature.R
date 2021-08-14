# ***************************************************************************************
# Library   : timetk
# Function  : step_timeseries_signature
# Created on: 2021/8/15
# URL       : https://business-science.github.io/timetk/reference/step_timeseries_signature.html
# ***************************************************************************************


# ＜概要＞
# - 日付インデックスから生成した日付要素を追加する


# ＜構文＞
# step_timeseries_signature(
#   recipe,
#   ...,
#   role = "predictor",
#   trained = FALSE,
#   columns = NULL,
#   skip = FALSE,
#   id = rand_id("timeseries_signature")
# )


# ＜目次＞
# 0 準備
# 1 レシピの作成
# 2 レシピサマリーの確認


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(recipes)
library(tidyverse)
library(tidyquant)
library(timetk)

# ローケール設定変更
Sys.setlocale("LC_TIME", "English")

# データ準備
FB_tbl <- FANG %>% filter(symbol == "FB")

# データ確認
FB_tbl %>% print()
FB_tbl %>% glimpse()


# 1 レシピの作成 -------------------------------------------------------------

# レシピ定義
# --- カレンダーを追加
rec_obj <-
  recipe(adjusted ~ ., data = FB_tbl) %>%
    step_timeseries_signature(date)

# 確認
rec_obj %>% prep() %>% juice()
rec_obj %>% prep() %>% juice() %>% glimpse()


# 2 レシピサマリーの確認 ---------------------------------------------------------

# サマリー
# --- 追加した日付要素が系列として表示される
rec_obj %>% summary()
rec_obj %>% prep() %>% summary()

# 処理内容の表示
# --- トップレベル
# --- 詳細レベル（日付要素が表示される）
rec_obj %>% prep() %>% tidy()
rec_obj %>% prep() %>% tidy(1)

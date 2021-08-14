# ***************************************************************************************
# Library   : timetk
# Function  : step_fourier
# Created on: 2021/8/15
# URL       : https://business-science.github.io/timetk/reference/step_fourier.html
# ***************************************************************************************


# ＜概要＞
# - フーリエ系列を追加するレシピステップ


# ＜構文＞
# step_fourier(
#   recipe,
#   ...,
#   period,
#   K,
#   role = "predictor",
#   trained = FALSE,
#   columns = NULL,
#   scale_factor = NULL,
#   skip = FALSE,
#   id = rand_id("fourier")
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


# データ準備
FB_tbl <- FANG %>%
  filter(symbol == "FB") %>%
  select(symbol, date, adjusted)

# データ確認
FB_tbl %>% print()
FB_tbl %>% glimpse()


# 1 レシピの作成 -------------------------------------------------------------

# レシピ定義
# --- フーリエ系列を追加
rec_obj <- 
  recipe(adjusted ~ ., data = FB_tbl) %>%
    step_fourier(date, period = c(252/4, 252), K = 2)

# 確認
rec_obj %>% prep() %>% juice()
rec_obj %>% prep() %>% juice() %>% glimpse()


# 2 レシピサマリーの確認 ---------------------------------------------------------

# サマリー
# --- 追加したフーリエ系列が表示される
rec_obj %>% summary()
rec_obj %>% prep() %>% summary()

# 処理内容の表示
# --- トップレベル
# --- 詳細レベル（フーリエ系列が表示される）
rec_obj %>% prep() %>% tidy()
rec_obj %>% prep() %>% tidy(1)

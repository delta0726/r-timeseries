# ***************************************************************************************
# Library   : timetk
# Function  : step_diff
# Created on: 2021/8/15
# URL       : https://business-science.github.io/timetk/reference/step_diff.html
# ***************************************************************************************


# ＜概要＞
# - 差分系列を追加するレシピステップ
#   --- timetk::diff_vec()をレシピ化したもの
#   --- recipes::step_lag()も同様の操作を行う


# ＜構文＞
# step_diff(
#   recipe,
#   ...,
#   role = "predictor",
#   trained = FALSE,
#   lag = 1,
#   difference = 1,
#   log = FALSE,
#   prefix = "diff_",
#   columns = NULL,
#   skip = FALSE,
#   id = rand_id("diff")
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
FANG_wide <-
  FANG %>%
    select(symbol, date, adjusted) %>%
    pivot_wider(names_from = symbol, values_from = adjusted)

# データ確認
FANG_wide %>% print()
FANG_wide %>% glimpse()


# 1 レシピの作成 -------------------------------------------------------------

# レシピ定義
# --- 差分系列の追加
recipe_diff <-
  recipe(~ ., data = FANG_wide) %>%
    step_diff(FB, AMZN, NFLX, GOOG, lag = 1:3, difference = 1)

# 確認
# --- 差分系列を作成した結果、NAがデータに含まれる点に注意（step_naomitで削除可能）
recipe_diff %>% prep() %>% bake(new_data = FANG_wide)
recipe_diff %>% prep() %>% bake(new_data = FANG_wide) %>% glimpse()


# 2 レシピサマリーの確認 ---------------------------------------------------------

# サマリー
# --- レシピ適用前
# --- レシピ適用後（差分系列が追加される）
recipe_diff %>% summary()
recipe_diff %>% prep() %>% summary()

# 処理内容の表示
# --- トップレベル
# --- 詳細レベル（差分系列が追加される）
recipe_diff %>% prep() %>% tidy()
recipe_diff %>% prep() %>% tidy(1)

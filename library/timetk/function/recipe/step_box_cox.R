# ***************************************************************************************
# Library   : timetk
# Function  : step_box_cox
# Created on: 2021/8/15
# URL       : https://business-science.github.io/timetk/reference/step_box_cox.html
# ***************************************************************************************


# ＜概要＞
# - Box-Cox変換を使用してデータを変換するレシピステップ
#   --- timetk::box_cox_vec()をレシピ化したもの
#   --- recipes :: step_BoxCox()とは異なる処理


# ＜特色＞
# - Guerreroラムダ最適化
# - 負の数値データの処理
# - 複数のメソッドを一括追加


# ＜構文＞
# step_box_cox(
#   recipe,
#   ...,
#   method = c("guerrero", "loglik"),
#   limits = c(-1, 2),
#   role = NA,
#   trained = FALSE,
#   lambdas_trained = NULL,
#   skip = FALSE,
#   id = rand_id("box_cox")
# )


# ＜目次＞
# 0 準備
# 1 レシピの作成
# 2 レシピサマリーの確認
# 3 データ比較


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
# --- 指定した系列にBox-Cox変換を適用
recipe_box_cox <-
  recipe(~ ., data = FANG_wide) %>%
    step_box_cox(FB, AMZN, NFLX, GOOG)

# 確認
recipe_box_cox %>% prep() %>% bake(new_data = FANG_wide)
recipe_box_cox %>% prep() %>% bake(new_data = FANG_wide) %>% glimpse()


# 2 レシピサマリーの確認 ---------------------------------------------------------

# サマリー
# --- 指定系列を変換しているので同じ結果
recipe_box_cox %>% summary()
recipe_box_cox %>% prep() %>% summary()

# 処理内容の表示
# --- トップレベル
# --- 詳細レベル（最適ラムダが表示される）
recipe_box_cox %>% prep() %>% tidy()
recipe_box_cox %>% prep() %>% tidy(1)


# 3 データ比較 -----------------------------------------------------------------

# レシピ適用前のデータ
FANG_wide_bef <-
  FANG_wide %>%
    mutate(type = "before")

# レシピ適用後のデータ
FANG_wide_aft <-
  recipe_box_cox %>%
    prep() %>%
    bake(new_data = FANG_wide) %>%
    mutate(type = "after")

# プロット比較
# --- 季節性
FANG_wide_bef %>%
  bind_rows(FANG_wide_aft) %>%
  pivot_longer(-c("date", "type")) %>%
  ggplot(aes(x = date, y = value, color = type)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y")

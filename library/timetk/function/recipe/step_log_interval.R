# ***************************************************************************************
# Library   : timetk
# Function  : step_log_interval
# Created on: 2021/8/15
# URL       : https://business-science.github.io/timetk/reference/step_log_interval.html
# ***************************************************************************************


# ＜概要＞
# - Log-Inerval変換を使用してデータを変換するレシピステップ
#   --- timetk::log_interval_vec()を使ったレシピ


# ＜構文＞
# step_log_interval(
#   recipe,
#   ...,
#   limit_lower = "auto",
#   limit_upper = "auto",
#   offset = 0,
#   role = NA,
#   trained = FALSE,
#   limit_lower_trained = NULL,
#   limit_upper_trained = NULL,
#   skip = FALSE,
#   id = rand_id("log_interval")
# )


# ＜目次＞
# 0 準備
# 1 既存列を更新
# 2 新規列で作成


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidyquant)
library(recipes)
library(timetk)


# データ準備
# --- 訓練データ
FANG_wide <- 
  FANG %>%
    select(symbol, date, adjusted) %>%
    pivot_wider(names_from = symbol, values_from = adjusted)

# データ確認
FANG_wide %>% print()
FANG_wide %>% glimpse()


# 1 準備 ---------------------------------------------------------------------

# レシピ定義
recipe_log_interval <- 
  recipe(~ ., data = FANG_wide) %>%
    step_log_interval(FB, AMZN, NFLX, GOOG, offset = 1)

# レシピ適用
recipe_log_interval %>% prep() %>% juice()

# レシピ確認
recipe_log_interval %>% tidy(1)



# 2 プロット比較 -----------------------------------------------------------

# レシピ適用前
FANG_wide %>%
  pivot_longer(-date) %>%
  plot_time_series(date, value, name, .smooth = FALSE, .interactive = FALSE)

# レシピ適用後
recipe_log_interval %>%
  prep() %>% 
  bake(FANG_wide) %>%
  pivot_longer(-date) %>%
  plot_time_series(date, value, name, .smooth = FALSE, .interactive = FALSE)

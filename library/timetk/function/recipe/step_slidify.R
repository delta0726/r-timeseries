# ***************************************************************************************
# Library   : timetk
# Function  : step_slidify
# Created on: 2021/8/24
# URL       : https://business-science.github.io/timetk/reference/step_slidify.html
# ***************************************************************************************


# ＜概要＞
# - カスタムで作成したローリング計算で系列を作成するレシピステップ


# ＜構文＞
# step_slidify(
#   recipe,
#   ...,
#   period,
#   .f,
#   align = c("center", "left", "right"),
#   partial = FALSE,
#   names = NULL,
#   role = "predictor",
#   trained = FALSE,
#   columns = NULL,
#   f_name = NULL,
#   skip = FALSE,
#   id = rand_id("slidify")
# )


# ＜目次＞
# 0 準備
# 1 既存列を更新
# 2 新規列で作成


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(recipes)
library(tidyverse)
library(tidyquant)
library(timetk)


# データ準備
# --- 訓練データ
FB_tbl <- 
  FANG %>%
  filter(symbol == "FB") %>%
  select(symbol, date, adjusted)

# データ準備
# --- 将来データ
new_data <- 
  FB_tbl %>%
  tail(90) %>%
  mutate(date = date %>% tk_make_future_timeseries(length_out = 90))

# データ確認
FB_tbl %>% print()
FB_tbl %>% glimpse()

# データ期間の確認
FB_tbl %>% tk_summary_diagnostics() %>% select(1:4)
new_data %>% tk_summary_diagnostics() %>% select(1:4)


# 1 既存列を更新 -------------------------------------------------------------

# レシピ定義
# --- ローリング系列の追加
rec_ma_50 <- 
  recipe(adjusted ~ ., data = FB_tbl) %>%
    step_slidify(adjusted, period = 50, .f = ~ AVERAGE(.x))

# レシピ適用
training_data_baked <- rec_ma_50 %>% prep() %>% juice()
new_data_baked      <- rec_ma_50 %>% prep() %>% bake(new_data)

# 確認
training_data_baked %>% print()
new_data_baked %>% print()

# プロット作成
training_data_baked %>%
  ggplot(aes(date, adjusted)) +
  geom_line() +
  geom_line(color = "red", data = new_data_baked)


# 2 新規列で作成 -------------------------------------------------------------

# レシピ定義
# --- ローリング系列の追加
rec_ma_30_names <- 
  recipe(adjusted ~ ., data = FB_tbl) %>%
    step_slidify(adjusted, period = 30, .f = AVERAGE, names = "adjusted_ma_30")

# レシピ適用
training_data_baked <- rec_ma_30_names %>% prep() %>% bake(FB_tbl)
new_data_baked      <- rec_ma_30_names %>% prep() %>% bake(new_data)

# 確認
training_data_baked %>% print()
new_data_baked %>% print()

# プロット作成
training_data_baked %>%
  ggplot(aes(date, adjusted)) +
  geom_line(alpha = 0.5) +
  geom_line(aes(y = adjusted_ma_30), color = "red", size = 1)

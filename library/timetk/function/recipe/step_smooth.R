# ***************************************************************************************
# Library   : timetk
# Function  : step_smooth
# Created on: 2021/8/15
# URL       : https://business-science.github.io/timetk/reference/step_smooth.html
# ***************************************************************************************



# ＜概要＞
# - LOESSを用いて系列をスムージングするレシピステップ
#   --- timetk::smooth_vec()を使用


# ＜構文＞
# step_smooth(
#   recipe,
#   ...,
#   period = 30,
#   span = NULL,
#   degree = 2,
#   names = NULL,
#   role = "predictor",
#   trained = FALSE,
#   columns = NULL,
#   skip = FALSE,
#   id = rand_id("smooth")
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


# 1 レシピの作成 -------------------------------------------------------------

# レシピ定義
# --- スムージング系列の追加
rec_smooth_period <- 
  recipe(adjusted ~ ., data = FB_tbl) %>%
    step_smooth(adjusted, period = 30)

# 確認
rec_smooth_period %>% prep() %>% juice()


# 2 レシピサマリーの確認 ---------------------------------------------------------

# サマリー
rec_smooth_period %>% summary()
rec_smooth_period %>% prep() %>% summary()

# 処理内容の表示
# --- トップレベル
# --- 詳細レベル（スムージングの設定が表示される）
rec_smooth_period %>% prep() %>% tidy()
rec_smooth_period %>% prep() %>% tidy(1)

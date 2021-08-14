# ***************************************************************************************
# Library   : timetk
# Function  : step_ts_impute
# Created on: 2021/8/15
# URL       : https://business-science.github.io/timetk/reference/step_ts_impute.html
# ***************************************************************************************


# ＜概要＞
# - 欠損値を追加するレシピステップ
#   --- recipes::ts_impute_vec()をレシピ化したもの


# ＜構文＞
# step_ts_impute(
#   recipe,
#   ...,
#   period = 1,
#   lambda = NULL,
#   role = NA,
#   trained = FALSE,
#   lambdas_trained = NULL,
#   skip = FALSE,
#   id = rand_id("ts_impute")
# )


# ＜引数＞
# - period : period = 1（季節性なし）
#          : period > 1（季節性あり）


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
# --- pad_by_time()でFiveDayをSevenDayに変更（NAを発生指せる）
FANG_wide <-
  FANG %>%
    select(symbol, date, adjusted) %>%
    pivot_wider(names_from = symbol, values_from = adjusted) %>%
    pad_by_time(.date_var = date)

# データ確認
# --- 土日がNAとして表示されていることを確認
FANG_wide %>% print()
FANG_wide %>% glimpse()


# 1 レシピの作成 -------------------------------------------------------------

# レシピ定義
# --- 指定した系列に欠損値補完を適用
# --- 季節性調整が適用されている（period = 252）
step_ts_impute <-
  recipe(~ ., data = FANG_wide) %>%
    step_ts_impute(FB, AMZN, NFLX, GOOG, period = 252, lambda = "auto")

# 確認
step_ts_impute %>% prep() %>% bake(new_data = FANG_wide)
step_ts_impute %>% prep() %>% bake(new_data = FANG_wide) %>% glimpse()


# 2 レシピサマリーの確認 ---------------------------------------------------------

# サマリー
# --- 指定系列を変換しているので同じ結果
step_ts_impute %>% summary()
step_ts_impute %>% prep() %>% summary()

# 処理内容の表示
# --- トップレベル
# --- 詳細レベル（最適ラムダが表示される）
step_ts_impute %>% prep() %>% tidy()
step_ts_impute %>% prep() %>% tidy(1)


# 3 データ比較 -----------------------------------------------------------------

# レシピ適用前のデータ
FANG_wide_bef <-
  FANG_wide %>%
    mutate(type = "before")

# レシピ適用後のデータ
FANG_wide_aft <-
  step_ts_impute %>%
    prep() %>%
    bake(new_data = FANG_wide) %>%
    mutate(type = "after")

# プロット比較
# --- 季節性
FANG_wide_bef %>%
  bind_rows(FANG_wide_aft) %>%
  filter_by_time(.start_date = "start", .end_date = "2013-12-31") %>%
  pivot_longer(-c("date", "type")) %>%
  filter(name == "FB") %>%
  ggplot(aes(x = date, y = value, color = type)) +
  geom_line()

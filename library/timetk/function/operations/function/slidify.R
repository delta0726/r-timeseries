# Title     : slidify
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/23
# URL       : https://business-science.github.io/timetk/reference/slidify.html



# ＜ポイント＞
# - ローリング計算のための関数を定義しる




# ＜構文＞
# slidify(
#   .f,
#   .period = 1,
#   .align = c("center", "left", "right"),
#   .partial = FALSE,
#   .unlist = TRUE
# )


# ＜引数＞
# - .align  : 移動平均の結果を出力する位置、通常は"right"がイメージに合う
# - partial : 要素数が足りない場合も計算するか




# 1.準備 -----------------------------------------------------

library(tidyverse)
library(tidyquant)
library(tidyr)
library(timetk)


# データ準備
data(FANG)
FB <- FANG %>% filter(symbol == "FB")


# 2.単純なローリング計算 -----------------------------------------------------

# 関数定義
# --- 移動平均
mean_roll_5  <- slidify(mean, .period = 5, .align = "right")
mean_roll_10 <- slidify(mean, .period = 10, .align = "right")

# 列追加
# --- 移動平均
FB %>%
  mutate(rolling_mean_5 = mean_roll_5(adjusted))


FB %>%
  select(symbol, date, adjusted) %>%
  mutate(rolling_mean_5  = mean_roll_5(adjusted),
         rolling_mean_10 = mean_roll_10(adjusted))



# 3.要素数が足りない箇所の出力 -----------------------------------------------------

# 関数定義
# --- 移動平均
mean_roll_5_partial <- slidify(mean, .period = 5, .align = "right", .partial = TRUE)


# 列追加
# --- 移動平均
FB %>%
  mutate(rolling_mean_5 = mean_roll_5_partial(adjusted))



# 4.複数のローリング系列を一度に追加 -----------------------------------------------------

FB %>%
  select(symbol, date, adjusted) %>%
  tk_augment_slidify(adjusted, .period = 5:10, .f = mean, .align = "right",
                     .names = str_c("MA_", 5:10))





# 5.グループごとにローリング計算 -----------------------------------------------------

# 定義
# --- 移動平均
mean_roll_3 <- slidify(mean, .period = 3, .align = "right")


# グループごとにローリング計算
FANG %>%
  group_by(symbol) %>%
  mutate(mean_roll = mean_roll_3(adjusted)) %>%
  slice(1:5)



# 6.複数引数を持つローリング計算 -----------------------------------------------------

# 定義
# --- ローリング相関係数
cor_roll <- slidify(~cor(.x, .y), .period = 5, .align = "right")


# ローリング相関係数
FB %>%
  mutate(running_cor = cor_roll(adjusted, open))



# 7.関数定義/ラムダ式で定義 -----------------------------------------------------


avg_of_avgs <- slidify(
    function(x, y, z) (mean(x) + mean(y) + mean(z)) / 3,
    .period = 10,
    .align = "right"
)

# Or
avg_of_avgs <- slidify(
    ~(mean(..1) + mean(..2) + mean(..3)) / 3,
    .period = 10,
    .align  = "right"
)

FB %>%
    mutate(avg_of_avgs = avg_of_avgs(open, high, low))




FB$adjusted[1] <- NA

roll_mean_na_rm <- slidify(~mean(.x, na.rm = TRUE), .period = 5, .align = "right")

FB %>%
    mutate(roll_mean = roll_mean_na_rm(adjusted))



# 7.ローリング回帰 -----------------------------------------------------

# 関数定義
lm_roll <- slidify(~lm(.x ~ .y), .period = 90, .unlist = FALSE, .align = "right")

FB %>%
    drop_na() %>%
    mutate(numeric_date = as.numeric(date)) %>%
    mutate(rolling_lm = lm_roll(adjusted, numeric_date)) %>%
    filter(!is.na(rolling_lm))




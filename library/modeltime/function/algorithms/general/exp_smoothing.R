# ***************************************************************************************
# Library   : modeltime
# Function  : exp_smoothing
# Created on: 2021/8/7
# URL       : https://business-science.github.io/modeltime/reference/exp_smoothing.html
# ***************************************************************************************


# ＜概要＞
# - 指数平滑化状態空間モデルのインターフェース
#   --- ETS    ：指数平滑法
#   --- CROSTON：Crostonの予測は、断続的な需要に対する指数平滑化の特殊なケース
#   --- THETA  ：M3コンペティションで好成績を収めたドリフトを伴う指数平滑化の特殊なケース


# ＜構文＞
# exp_smoothing(
#   mode = "regression",
#   seasonal_period = NULL,
#   error = NULL,
#   trend = NULL,
#   season = NULL,
#   damping = NULL,
#   smooth_level = NULL,
#   smooth_trend = NULL,
#   smooth_seasonal = NULL
# )


# ＜引数＞


# ＜使用例＞
# 0 準備
# 1 AUTO ETS
# 2 STANDARD ETS
# 3 CROSTON
# 4 THETA


# 0 準備 --------------------------------------------------------------------------------

# ライブラリ
library(dplyr)
library(parsnip)
library(rsample)
library(timetk)
library(modeltime)


# データ準備
# --- 1系列を抽出
m750 <- m4_monthly %>% filter(id == "M750")

# データ確認
m750 %>% print()

# データ分割
splits <- m750 %>% initial_time_split(prop = 0.8)


# 1 AUTO ETS -------------------------------------------------------------------------

# モデル構築
model_spec <-
  exp_smoothing() %>%
    set_engine("ets")

# 学習
model_fit <-
  model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# 確認
model_spec %>% print()
model_fit %>% print()


# 2 STANDARD ETS ----------------------------------------------------------------------

# モデル構築
model_spec <-
  exp_smoothing(seasonal_period  = 12,
                error            = "multiplicative",
                trend            = "additive",
                season           = "multiplicative") %>%
    set_engine("ets")

# 学習
model_fit <-
  model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# 確認
model_spec %>% print()
model_fit %>% print()


# 3 CROSTON ----------------------------------------------------------------------------

# モデル構築
model_spec <-
  exp_smoothing(smooth_level = 0.2) %>%
    set_engine("croston")

# 学習
model_fit <-
  model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# 確認
model_spec %>% print()
model_fit %>% print()


# 4 THETA -----------------------------------------------------------------------------

# モデル構築
model_spec <-
  exp_smoothing() %>%
    set_engine("theta")

# 学習
model_fit <-
  model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# 確認
model_spec %>% print()
model_fit %>% print()

# ***************************************************************************************
# Library   : modeltime
# Function  : window_reg
# Created on: 2021/8/7
# URL       : https://business-science.github.io/modeltime/reference/window_reg.html
# ***************************************************************************************


# ＜概要＞
# - ウィンドウ予測モデルの一般的なインターフェイス


# ＜構文＞
# window_reg(mode = "regression", id = NULL, window_size = NULL)


# ＜引数＞


# ＜使用例＞
# 0 準備
# 1 WINDOW FUNCTION
# 2 PANEL FORECAST - WINDOW FUNCTION
# 3 BROADCASTING PANELS (REPEATING)


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


# 1 WINDOW FUNCTION ---------------------------------------------------------------------

# モデル構築
model_spec <-
  window_reg(window_size     = 12) %>%
    set_engine(engine          = "window_function",
               window_function = median,
               na.rm           = TRUE)

# 学習
model_fit <-
  model_spec %>%
    fit(log(value) ~ date, data = training(splits))

# 確認
model_spec %>% print()
model_fit %>% print()

# 予測
model_fit %>% predict(testing(splits))


# 2 PANEL FORECAST - WINDOW FUNCTION ----------------------------------------------------

# モデル構築
model_spec <-
  window_reg(id          = "id",
             window_size = 12) %>%
    set_engine(engine = "window_function",
               window_function = ~ sum(tail(.x, 3) * c(0.1, 0.3, 0.6)))

# 学習
model_fit <-
  model_spec %>%
    fit(log(value) ~ date + id, data = training(splits))

# 確認
model_spec %>% print()
model_fit %>% print()

# 予測
model_fit %>% predict(testing(splits))


# 3 BROADCASTING PANELS (REPEATING) -----------------------------------------------------

# モデル構築
model_spec <-
  window_reg(id          = "id",
             window_size = Inf) %>%
    set_engine(engine          = "window_function",
               window_function = ~ tail(.x, 12))

# 学習
model_fit <-
  model_spec %>%
    fit(log(value) ~ date + id, data = training(splits))

# 確認
model_spec %>% print()
model_fit %>% print()

# 予測
model_fit %>% predict(testing(splits))

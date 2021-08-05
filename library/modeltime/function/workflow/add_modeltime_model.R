# ***************************************************************************************
# Library   : modeltime
# Function  : add_modeltime_model
# Created on: 2021/8/6
# URL       : https://business-science.github.io/modeltime/reference/add_modeltime_model.html
# ***************************************************************************************


# ＜概要＞
# - 既存のモデルテーブルにモデルを追加する


# ＜構文＞
# add_modeltime_model(object, model, location = "bottom")


# ＜使用例＞

# ライブラリ
library(modeltime)
library(tidymodels)

# 既存のモデルテーブル
m750_models %>% print()

# 新しいモデルの作成
# --- etsモデル
model_fit_ets <-
  exp_smoothing() %>%
    set_engine("ets") %>%
    fit(value ~ date, training(m750_splits))

# モデルテーブルに追加
m750_models %>%
  add_modeltime_model(model_fit_ets)

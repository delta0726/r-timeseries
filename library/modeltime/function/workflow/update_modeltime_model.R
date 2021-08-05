# ***************************************************************************************
# Library   : modeltime
# Function  : update_modeltime_model
# Created on: 2021/8/6
# URL       : https://business-science.github.io/modeltime/reference/update_modeltime_model.html
# ***************************************************************************************


# ＜概要＞
# - 既に登録されているモデルIDのモデルを新しいモデルに更新する


# ＜構文＞
# update_modeltime_model(object, .model_id, .new_model)


# ＜使用例＞

# ライブラリ
library(tidymodels)
library(modeltime)

# 既存のモデルテーブルの確認
m750_models %>% print()

# モデル定義＆学習
model_fit_ets <-
  exp_smoothing() %>%
    set_engine("ets") %>%
    fit(value ~ date, training(m750_splits))

# モデルテーブルのモデル更新
m750_models %>%
    update_modeltime_model(1, model_fit_ets)

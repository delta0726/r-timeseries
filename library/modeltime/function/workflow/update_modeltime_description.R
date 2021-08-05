# ***************************************************************************************
# Library   : modeltime
# Function  : update_model_description / update_modeltime_description
# Created on: 2021/8/6
# URL       : https://business-science.github.io/modeltime/reference/update_model_description.html
# ***************************************************************************************


# ＜概要＞
# - モデルテーブルのディスクリプションを変更する


# ＜構文＞
# update_model_description(object, .model_id, .new_model_desc)
# update_modeltime_description(object, .model_id, .new_model_desc)


# ＜使用例＞

# ライブラリ
library(modeltime)
library(magrittr)

# モデルテーブルの確認
m750_models

# 概要の更新
m750_models %>%
  update_model_description(2, "PROPHET - No Regressors")

# 概要の更新
m750_models %>%
  update_modeltime_description(2, "PROPHET - No Regressors")

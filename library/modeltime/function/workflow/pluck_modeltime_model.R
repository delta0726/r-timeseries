# ***************************************************************************************
# Library   : modeltime
# Function  : pluck_modeltime_model
# Created on: 2021/8/6
# URL       : https://business-science.github.io/modeltime/reference/pluck_modeltime_model.html
# ***************************************************************************************


# ＜概要＞
# - モデルテーブルからモデルを指定して抽出する


# ＜構文＞
# pluck_modeltime_model(object, .model_id)


# ＜使用例＞

# ライブラリ
library(modeltime)
library(magrittr)

# モデルテーブルの確認
m750_models

# モデル抽出
m750_models %>% pluck_modeltime_model(2)

# ***************************************************************************************
# Library   : modeltime
# Function  : pull_modeltime_model
# Created on: 2021/8/6
# URL       : https://business-science.github.io/modeltime/reference/pluck_modeltime_model.html
# ***************************************************************************************


# ＜概要＞
# - モデルテーブルからモデルを指定して抽出する
#   --- pluck_modeltime_model()を参照
#   --- {workflow}に合わせてpull_*としたと思われる


# ＜構文＞
# pull_modeltime_model(object, .model_id)


# ＜使用例＞

# ライブラリ
library(modeltime)
library(magrittr)

# モデルテーブルの確認
m750_models

# モデル抽出
m750_models %>% pull_modeltime_model(2)




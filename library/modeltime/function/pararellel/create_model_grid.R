# ***************************************************************************************
# Library   : modeltime
# Function  : create_model_grid
# Created on: 2021/8/5
# URL       : https://business-science.github.io/modeltime/reference/create_model_grid.html
# ***************************************************************************************


# ＜概要＞
# - パラメータグリッドからモデルテーブルを作成するためのヘルパー
#   --- 関数内で直接モデルを定義する（別途作ったモデルを登録するのではない）


# ＜構文＞
# create_model_grid(grid, f_model_spec, engine_name, ..., engine_params = list())


# ＜引数＞
# grid         ：パラメータグリッド(tibble形式)
# f_model_spec ：parsnipモデル
# engine_name  ：エンジン名（parsnip::set_engine()に対応）
# ...          ：parsnipモデルの引数
# engine_params：エンジンで指定した関数固有のパラメータ


# ＜使用例＞
# 0 準備
# 1 グリッドテーブルの作成


# 0 準備 ------------------------------------------------------------------------------

# ライブラリ
library(tidymodels)
library(modeltime)



# 1 グリッドテーブルの作成 ---------------------------------------------------------------

# チューニンググリッドの作成
# --- {dials}のパラメータで指定した範囲からグリッドを作成
grid_tbl <-
  grid_regular(learn_rate(),
               levels = 3)

# グリッドにモデルを追加
model_tables <-
  grid_tbl %>%
    create_model_grid(f_model_spec  = boost_tree,
                      engine_name   = "xgboost",
                      mode          = "regression",
                      engine_params = list(max_depth = 5))

# 確認
# --- チューニングパラメータが変更されたモデルになっている
model_tables$.models

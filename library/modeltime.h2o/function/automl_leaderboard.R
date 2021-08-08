# ***************************************************************************************
# Library   : modeltime.h2o
# Function  : automl_leaderboard
# Created on: 2021/8/8
# URL       : https://business-science.github.io/modeltime.h2o/reference/automl_leaderboard.html
# ***************************************************************************************


# ＜概要＞
# - h2o::automl()のオブジェクトからリーダーボードを取得する


# ＜構文＞
# automl_leaderboard(object)


# ＜使用例＞
# 0 準備
# 1 AutoMLの実行
# 2 リーダーボードの確認
# 3 出力モデルの更新
# 4 終了処理


# 0 準備 ------------------------------------------------------------------------------------

# ライブラリ
library(tidymodels)
library(modeltime.h2o)
library(h2o)
library(tidyverse)
library(timetk)


# オブジェクト確認
m750_splits %>% print()



# 1 AutoMLの実行 ----------------------------------------------------------------------------

# H2O起動
h2o.init(nthreads = -1, ip = 'localhost', port = 54321)

# モデル構築
model_spec <- 
  automl_reg(mode = 'regression') %>%
  set_engine(engine                     = 'h2o', 
             max_runtime_secs           = 5, 
             max_runtime_secs_per_model = 4, 
             nfolds                     = 5, 
             max_models                 = 3, 
             exclude_algos              = c("DeepLearning"), 
             seed                       = 786) 


# 学習
# --- AutoML
model_fit <- 
  model_spec %>%
    fit(value ~ ., data = training(m750_splits))



# 2 リーダーボードの確認 ---------------------------------------------------------------------

# リーダーボードの取得
leaderboard_tbl <- model_fit %>% automl_leaderboard()

# 確認
leaderboard_tbl %>% print()



# 3 出力モデルの更新 ------------------------------------------------------------------------

# ＜ポイント＞
# - AutoMLではリーダーボードの最上位モデルが出力される
#  --- 個別モデルを取得することで、他のモデルっ出の予測も可能

# モデルIDの取得
model_id_2  <- leaderboard_tbl$model_id[[2]]
model_id_2 %>% print()

# モデル取得
model_fit_2 <- model_fit %>% automl_update_model(model_id_2)
model_fit_2 %>% print()


# 4 終了処理 -------------------------------------------------------------------------------

# H2O終了
h2o.shutdown(prompt = FALSE)


# ***************************************************************************************
# Library   : modeltime.h2o
# Function  : automl_reg
# Created on: 2021/8/8
# URL       : https://business-science.github.io/modeltime.h2o/reference/automl_reg.html
# ***************************************************************************************


# ＜概要＞
# - h2oのautomlを{parsnip}と{modeltable}のフレームワークで使用するためのラッパー関数
#   --- 時系列予測で使用するため回帰モードのみ対応


# ＜構文＞
# - automl_reg(mode = "regression")



# ＜使用例＞
# 0 準備
# 1 データ分割
# 2 特徴量エンジニアリング
# 3 AutoMLの定義
# 4 AutoMLの実行
# 5 終了処理


# 0 準備 --------------------------------------------------------------------------------

# ライブラリ
library(tidymodels)
library(modeltime.h2o)
library(h2o)
library(tidyverse)
library(timetk)


# ローケール設定変更
Sys.setlocale("LC_TIME", "English")
Sys.getlocale("LC_TIME")

# データ確認
walmart_sales_weekly %>% print()


# データ準備
data_tbl <- 
  walmart_sales_weekly %>%
    select(id, Date, Weekly_Sales)


# 1 データ分割 -----------------------------------------------------------------------

# 時系列データ分割
splits <- 
  data_tbl %>% 
  time_series_split(assess     = "3 month", 
                    cumulative = TRUE)

# データ期間の確認
splits %>% training() %>% tk_summary_diagnostics() %>% select(1:4)
splits %>% testing() %>% tk_summary_diagnostics() %>% select(1:4)



# 2 特徴量エンジニアリング -----------------------------------------------------------

# レシピ定義
recipe_spec <- 
  recipe(Weekly_Sales ~ ., data = training(splits)) %>%
    step_timeseries_signature(Date)

# データ確認
recipe_spec %>% prep() %>% juice() %>% glimpse()

# 学習用データの定義
train_tbl <- recipe_spec %>% prep() %>% bake(training(splits))
test_tbl  <- recipe_spec %>% prep() %>% bake(testing(splits))


# 3 AutoMLの定義 --------------------------------------------------------------------

# H2Oの初期化
h2o.init(nthreads = -1, ip = 'localhost', port = 54321)


# モデル定義
model_spec <- 
  automl_reg(mode = 'regression') %>%
  set_engine(engine                     = 'h2o',
             max_runtime_secs           = 30, 
             max_runtime_secs_per_model = 30,
             project_name               = 'project_01',
             nfolds                     = 5,
             max_models                 = 1000,
             exclude_algos              = c("DeepLearning"),
             seed                       =  786)

# 確認
model_spec %>% print()


# 4 AutoMLの実行 -------------------------------------------------------------------

# 学習
# --- This training process should take 30-40 seconds
model_fitted <- 
  model_spec %>%
    fit(Weekly_Sales ~ ., data = train_tbl)


# 確認
model_fitted %>% print()


# 予測
model_fitted %>% predict(test_tbl)


# 5 終了処理 ----------------------------------------------------------------------

# 終了処理
h2o.shutdown(prompt = FALSE)

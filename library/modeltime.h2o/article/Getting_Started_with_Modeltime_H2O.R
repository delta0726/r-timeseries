# ******************************************************************************
# Library     : modeltime.h2o
# Theme       : Getting Started with Modeltime H2O
# Update Date : 2021/8/8
# URL         : https://business-science.github.io/modeltime.h2o/articles/getting-started.html
# ******************************************************************************


# ＜概要＞
# - AutoMLを{tidymodels}と{modeltime}のフレームワークで扱うフローを確認する


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 AutoMLの準備
# 3 AutoMLの実行
# 4 {modeltable}のワークフロー
# 5 リフィット
# 6 終了処理


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidymodels)
library(modeltime.h2o)
library(tidyverse)
library(timetk)


# ローケール設定変更
Sys.setlocale("LC_TIME", "English")
Sys.getlocale("LC_TIME")


# データ準備
data_tbl <- 
  walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales)

# データ確認
data_tbl %>% print()
data_tbl %>% group_by(id) %>% tally()

# プロット確認
data_tbl %>% 
  group_by(id) %>% 
  plot_time_series(.date_var    = Date, 
                   .value       = Weekly_Sales, 
                   .facet_ncol  = 2, 
                   .smooth      = F, 
                   .interactive = F)


# 1 データ準備 -----------------------------------------------------------------

# 時系列データ分割
splits <- 
  data_tbl %>% 
    time_series_split(assess = "3 month", cumulative = TRUE)


# 特徴量エンジニアリング
recipe_spec <- 
  recipe(Weekly_Sales ~ ., data = training(splits)) %>%
    step_timeseries_signature(Date) 

# データ作成
# --- 訓練データ
# --- 検証データ
train_tbl <- recipe_spec %>% prep() %>% bake(training(splits)) 
test_tbl  <- recipe_spec %>% prep() %>% bake(testing(splits)) 


# 2 AutoMLの準備 -------------------------------------------------------------

# H2Oの起動
h2o.init(nthreads = -1, ip = 'localhost', port = 54321)

# H2Oの設定
h2o.no_progress()

# モデル構築
model_spec <- 
  automl_reg(mode = 'regression') %>%
    set_engine(engine                     = 'h2o', 
               max_runtime_secs           = 5,
               max_runtime_secs_per_model = 3, 
               max_models                 = 3, 
               nfolds                     = 5, 
               exclude_algos              = c("DeepLearning"), 
               verbosity                  = NULL, 
               seed                       = 786) 

# 確認
model_spec %>% print()


# 3 AutoMLの実行 -------------------------------------------------------------

# 学習
# --- AutoMLの実行
model_fitted <- 
  model_spec %>%
    fit(Weekly_Sales ~ ., data = train_tbl)

# 確認
# --- プリント出力
# --- リーダーボード
model_fitted %>% print()
model_fitted %>% automl_leaderboard()


# 予測
# --- 最良モデルで予測が作成される
model_fitted %>% predict(test_tbl)


# 4 {modeltable}のワークフロー ------------------------------------------------

# テーブル登録
modeltime_tbl <- 
  modeltime_table(model_fitted) 

# 確認
modeltime_tbl

# 予測データのプロット
# --- キャリブレーション
# --- 予測
# --- プロット作成
modeltime_tbl %>%
  modeltime_calibrate(test_tbl) %>%
  modeltime_forecast(new_data    = test_tbl, 
                     actual_data = data_tbl, 
                     keep_data   = TRUE) %>%
  group_by(id) %>%
  plot_modeltime_forecast(.facet_ncol = 2,
                          .interactive = FALSE)


# 5 リフィット ----------------------------------------------------------------

# データ結合
# --- 訓練データ + 検証データ
data_prepared_tbl <- 
  train_tbl %>% 
  bind_rows(test_tbl)

# 将来データの作成
future_tbl <- 
  data_prepared_tbl %>%
    group_by(id) %>%
    future_frame(.length_out = "1 year") %>%
    ungroup()

# データ加工
# --- 特徴量エンジニアリングの適用
future_prepared_tbl <- 
  recipe_spec %>% 
    prep() %>% 
    bake(future_tbl)

# リフィット
# --- 新しいデータで再学習
refit_tbl <- 
  modeltime_tbl %>%
    modeltime_refit(data_prepared_tbl)

# 予測データのプロット
# --- キャリブレーション
# --- 予測
# --- プロット作成
refit_tbl %>%
  modeltime_forecast(new_data    = future_prepared_tbl, 
                     actual_data = data_prepared_tbl, 
                     keep_data   = TRUE) %>%
  group_by(id) %>%
  plot_modeltime_forecast(.facet_ncol  = 2, 
                          .interactive = FALSE)


# 6 終了処理 ------------------------------------------------------------------

# H2Oの終了
h2o.shutdown(prompt = FALSE)


# ***************************************************************************************
# Library     : modeltime.resample
# Theme       : Getting Started with Modeltime Resample
# Update Date : 2021/8/8
# URL         : https://business-science.github.io/modeltime.resample/articles/getting-started.html
# ******************************************************************************


# ＜概要＞
# - 1系列データ複数モデルのモデルテーブルに対して時系列クロスバリデーションを行う
# - クロスバリデーションは予測精度の安定性を評価するプロセス
#   --- クロスセクションの場合クロスバリデーションで予測値を取得することも可能（予測の平均値）
#   --- 時系列の場合、データが複数Foldで用いられる可能性があるので予測には不向きか？


# ＜用語整理＞
# - リサンプリング     ：データセットをFoldに分割するプロセス
# - クロスバリデーション：モデルを用いてリサンプリングデータを個別に学習＆評価するプロセス


# ＜目次＞
# 0 準備
# 1 リサンプリングデータの作成
# 2 モデルテーブルの作成
# 3 クロスバリデーションで学習
# 4 予測精度の評価


# 0 準備 ---------------------------------------------------------------------------

# ライブラリ
library(tidymodels)
library(modeltime)
library(modeltime.resample)
library(tidyverse)
library(timetk)


# データ確認
m750 %>% print()

# データ確認
m750 %>%
  plot_time_series(date, value, .interactive = FALSE)


# 1 リサンプリングデータの作成 ----------------------------------------------------------

# ＜ポイント＞
# - 時系列リサンプリングデータは開始時点を複数地点設定して、指定した期間でAnalysis/Assessmentデータに分ける
#   --- クロスセクションではデータ重複がないように分割したが、時系列では重複を許容する


# データ分割
resamples_tscv <-
  m750 %>%
    time_series_cv(assess      = "2 years",
                   initial     = "5 years",
                   skip        = "2 years",
                   slice_limit = 4)

# 確認
resamples_tscv %>% print()

# 可視化
resamples_tscv %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .facet_ncol = 2, .interactive = FALSE)


# 2 モデルテーブルの作成 ----------------------------------------------------------------

# 確認
# --- サンプルオブジェクトが用意されている
m750_models %>% print()


# 3 クロスバリデーションで学習 -----------------------------------------------------------

# 学習
resamples_fitted <-
  m750_models %>%
    modeltime_fit_resamples(resamples = resamples_tscv,
                            control   = control_resamples(verbose = FALSE))

# 確認
# --- .resample_resultsに結果が格納
resamples_fitted %>% print()


# 4 予測精度の評価 ---------------------------------------------------------------------

# テーブル表示
resamples_fitted %>%
  modeltime_resample_accuracy(summary_fns = mean) %>%
  table_modeltime_accuracy(.interactive = FALSE)

# 可視化
resamples_fitted %>%
  plot_modeltime_resamples(.point_size  = 3,
                           .point_alpha = 0.8,
                           .interactive = FALSE)

# ***************************************************************************************
# Library   : modeltime
# Function  : modeltime_fit_workflowset
# Created on: 2021/8/7
# URL       : https://business-science.github.io/modeltime/reference/modeltime_fit_workflowset.html
# ***************************************************************************************


# ＜概要＞
# workflowsetオブジェクトを受け取り、各モデルを1つまたは複数の時系列に順次または並列に適合させるfit())のラッパー関数


# ＜構文＞
# modeltime_fit_workflowset(
#   object,
#   data,
#   ...,
#   control = control_fit_workflowset()
# )


# ＜使用例＞
# 0 準備
# 1 レシピ作成
# 2 モデル構築
# 3 ワークフローセットの作成
# 4 学習およびモデルテーブル変換


# 0 準備 ---------------------------------------------------------------------------------------

# ライブラリ
library(tidymodels)
library(modeltime)
library(workflowsets)
library(tidyverse)
library(lubridate)
library(timetk)

# データ準備
data_set <- m4_monthly

# データ確認
# --- パネルデータ
data_set %>% print()


# 1 レシピ作成 -----------------------------------------------------------------------------------

# レシピ定義
rec1 <-
  recipe(value ~ date + id, data_set) %>%
    step_mutate(date_num = as.numeric(date)) %>%
    step_mutate(month_lbl = lubridate::month(date, label = TRUE)) %>%
    step_dummy(all_nominal(), one_hot = TRUE)

# データ確認
rec1 %>% prep() %>% juice() %>% glimpse()


# 2 モデル構築 ------------------------------------------------------------------------------------

# ＜ポイント＞
# - 未学習モデルを定義する
#   --- ワークフローセットでは未学習モデルを使用
#   --- モデルテーブルに直接登録する場合は学習済モデルを使用

# モデル1
# --- 線形回帰モデル
mod1 <-
  linear_reg() %>%
    set_engine("lm")

# モデル2
# --- プロフェットモデル
mod2 <-
  prophet_reg() %>%
    set_engine("prophet")


# 3 ワークフローセットの作成 ------------------------------------------------------------------------

# ＜ポイント＞
# - レシピとモデルの組み合わせを作成する

# ワークフローセットの定義
wfsets <-
  workflow_set(preproc = list(rec1 = rec1),
               models  = list(mod1 = mod1, mod2 = mod2),
               cross   = TRUE)

# 確認
# --- ワークフローセットのオブジェクト
wfsets %>% print()


# 4 学習およびモデルテーブル変換 --------------------------------------------------------------------

# 学習
# --- 本来はデータ分割して検証データで学習する
model_tbl <-
  wfsets %>%
    modeltime_fit_workflowset(data = data_set)

# 確認
# --- モデルテーブルに変換されている
# --- キャリブレーションを行って以降のプロセスを行う
model_tbl %>% print()
model_tbl %>% class()

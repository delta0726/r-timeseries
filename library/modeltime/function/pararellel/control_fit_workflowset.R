# ***************************************************************************************
# Library   : modeltime
# Function  : control_fit_workflowset
# Created on: 2021/8/5
# URL       : https://business-science.github.io/modeltime/reference/control_fit_workflowset.html
# ***************************************************************************************


# ＜概要＞
# - モデルテーブルにおける並列処理の詳細を指定する
#   --- 設定内容がprintで表示されるので分かりやすい


# ＜構文＞
# control_fit_workflowset(
#   verbose = FALSE,
#   allow_par = FALSE,
#   cores = -1,
#   packages = NULL
# )


# ＜使用例＞

# 並列処理なし
# --- 通常は並列処理をするために用いるのでallow_par引数はTRUEにする
control_fit_workflowset()

# 並列処理あり
control_fit_workflowset(allow_par = TRUE)

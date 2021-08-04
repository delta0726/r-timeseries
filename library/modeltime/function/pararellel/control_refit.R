# ***************************************************************************************
# Library   : modeltime
# Function  : control_refit
# Created on: 2021/8/5
# URL       : https://business-science.github.io/modeltime/reference/control_refit.html
# ***************************************************************************************


# ＜概要＞
# - modeltime_refit()のプロセスを制御する内部関数
#   --- ｢refit｣とは、モデルテーブルの条件を変更した際に再学習すること
#   --- refitの際に並列処理を行うための設定

# ＜構文＞
# control_refit(verbose = FALSE, allow_par = FALSE, cores = -1, packages = NULL)


# ＜使用例＞

# 並列処理なし
# --- 通常は並列処理をするために用いるのでallow_par引数はTRUEにする
control_refit()

# 並列処理あり
control_refit(allow_par = TRUE)

# ***************************************************************************************
# Library   : modeltime
# Function  : parallel_start / parallel_stop
# Created on: 2021/8/5
# URL       : https://business-science.github.io/modeltime/reference/parallel_start.html
# ***************************************************************************************


# ＜概要＞
# - モデルテーブルを並列処理する


# ＜構文＞
# parallel_start(...)
# parallel_stop()


# ＜引数＞
# ...：Parameters passed to parallel::makeCluster()


# ＜詳細＞
# - 3つのステップを実行している
#   --- 関数の中身を見ることで確認することができる

# 1 クラスターを作成
# 2 クラスターを登録
# 3 .libPaths()を追加


# ＜使用例＞

# 並列処理の開始
parallel_start(2)

# 並列処理の終了
parallel_stop()

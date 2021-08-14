# ***************************************************************************************
# Library   : timetk
# Function  : get_tk_time_scale_template
# Created on: 2021/8/8
# URL       : https://business-science.github.io/timetk/reference/tk_time_scale_template.html
# ***************************************************************************************


# ＜ポイント＞
# - 日付に対するトレンド分析等の頻度を指定
# - タイムスケールテンプレートはtk_get_frequency()やtk_get_trend()でperiod="auto"とした際に利用される


# ＜構文＞
# get_tk_time_scale_template()


# ＜目次＞
# 0 準備
# 1 テンプレート確認


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(timetk)


# 1 テンプレート確認 ----------------------------------------------------------

# 確認
get_tk_time_scale_template()

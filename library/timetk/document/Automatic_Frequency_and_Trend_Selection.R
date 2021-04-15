# Title     : Automatic Frequency and Trend Selection
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/5
# URL       : https://business-science.github.io/timetk/articles/TK06_Automatic_Frequency_And_Trend_Selection.html


# ＜ポイント＞
# - 頻度とトレンドサイクルは、SARIMA予測やSTL分解など多くの時系列分析で使用される
# - timetkは自動周波数およびトレンド選択ツールを備えている



# 1.準備 -----------------------------------------------------------------------


library(tidyverse)
library(tidyquant)
library(timetk)


# データ準備
data("FANG")


# データ確認
FANG %>% print()
FANG %>% glimpse()


# デイリーデータ
# --- FBのみ抽出
FB_tbl <- FANG %>% filter(symbol == "FB")
FB_tbl %>% print()


# サブデイリーデータ
# --- 30分毎
taylor_30_min




# 2.日次データ -----------------------------------------------------------------------

# STL分解
# --- adjusted
FB_tbl %>%
  plot_stl_diagnostics(date, adjusted,
                       .frequency = "auto", .trend = "auto",
                       .interactive = FALSE)


# STL分解
# --- volume
FB_tbl %>%
  plot_stl_diagnostics(date, volume,
                       .frequency = "auto", .trend = "auto",
                       .interactive = FALSE)



# 頻度を取得
FB_tbl %>% tk_index() %>% tk_get_frequency(period = "auto")


# トレンドを取得
FB_tbl %>% tk_index() %>% tk_get_trend(period = "auto")





# 3.30分データ -----------------------------------------------------------------------

taylor_30_min %>%
  plot_stl_diagnostics(date, value,
                       .frequency = "auto", .trend = "auto",
                       .interactive = FALSE)


# 頻度を取得
taylor_30_min %>% tk_index() %>% tk_get_frequency("1 day")


# トレンドを取得
taylor_30_min %>% tk_index() %>% tk_get_trend("auto")



# 4.時間スケールのテンプレート -------------------------------------------------------------

# テンプレートの確認
get_tk_time_scale_template()




# Title     : Time Series Data Wrangling
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/23
# URL       : https://business-science.github.io/timetk/articles/TK07_Time_Series_Data_Wrangling.html


# ＜ポイント＞





# 1 準備 --------------------------------------------------------------------------

library(tidyverse)
library(tidyquant)
library(timetk)
library(vctrs)


# データ確認
# --- 4銘柄の四本値と売買高のデータ
FANG %>% print()
FANG %>% glimpse()
FANG %>% class()


# カテゴリ確認
FANG %>% group_by(symbol) %>% tally()


# プロット作成
# --- 終値(adjusted)
FANG %>%
  group_by(symbol) %>%
  plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE)


# プロット作成
# --- 売買高(adjusted)
FANG %>%
  group_by(symbol) %>%
  plot_time_series(date, volume, .facet_ncol = 2, .interactive = FALSE)




# 2 時間ごとのサマリー集計 --------------------------------------------------------

# ＜ポイント＞
# - summarise_by_time()を使うことで、時間単位の集計が簡単に行える
# - 集計に用いているSUM()は{tidyquant}が定義する関数


# ＜集計関数＞
# SUM(x)
# AVERAGE(x)
# MEDIAN(x)
# MIN(x)
# MAX(x)
# COUNT(x)
# COUNT_UNIQUE(x)
# STDEV(x)
# VAR(x)
# COR(x, y)
# COV(x, y)
# FIRST(x)
# LAST(x)
# NTH(x, n = 1) ※n番目
# CHANGE_FIRSTLAST(x)
# PCT_CHANGE_FIRSTLAST(x)


# プロット作成
FANG %>%
  group_by(symbol) %>%
  summarise_by_time(date, .by = "quarter", volume = SUM(volume)) %>%
  plot_time_series(date, volume, .facet_ncol = 2, .interactive = FALSE, .y_intercept = 0)



# 3 時系列スムージング --------------------------------------------------------

# ＜ポイント＞
# - データ頻度を減らすことでスムージングすることができる
#   --- ここではFIRST()を適用することで頻度を減らしている
#   --- ローリング関数を適用する方法もある


FANG %>%
  group_by(symbol) %>%
  summarise_by_time(date, .by = "month", adjusted = FIRST(adjusted) ) %>%
  plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE)




# 4 時間でフィルタリング --------------------------------------------------------

# ＜ポイント＞
# - filter_by_time()を使うことで自然に日付のフィルタリングを行うことができる


FANG %>%
  group_by(symbol) %>%
  filter_by_time(date, "2013-09", "2013") %>%
  plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE)





# 5 欠損日付の補完 --------------------------------------------------------

# ＜ポイント＞
# - 欠損した日付を補完する
#   --- FiveDayからSevenDayに変換する場合などに便利
#   --- 日次分析の場合、日付ラグが大きいことで不都合が生じることもある


# 元データの確認
# --- 土曜日/日曜日が欠損している
FANG %>% print()


# 欠損日付を補完
# --- 土曜日/日曜日がNAとして表示される
FANG %>%
  group_by(symbol) %>%
  pad_by_time(date, .by = "auto")



# 低頻度から高頻度に変換
# --- 欠損値は線形補完
FANG %>%
  group_by(symbol) %>%
  pad_by_time(date, .by = "hour") %>%
  mutate_at(vars(open:adjusted), .funs = ts_impute_vec, period = 1) %>%
  filter_by_time(date, "start", FIRST(date) %+time% "1 month") %>%
  plot_time_series(date, adjusted, .facet_ncol = 2, .interactive = FALSE)



# 6 ローリング計算 --------------------------------------------------------

# ＜ポイント＞
# - ローリング関数を自分で定義して適用することができる
#   --- tibbletime::rollify()の拡張版の関数



# ***** 移動平均 ***************************************************


# Make the rolling function
roll_avg_30 <- slidify(.f = AVERAGE, .period = 30, .align = "center", .partial = TRUE)


# データ確認
# --- エラー発生
#Error (Time Series Data Wrangling.R#196): Problem with `mutate()` input `rolling_lm`.
#x .onLoad は loadNamespace()（'slider' に対する）の中で失敗しました、詳細は:
#call: fun(libname, pkgname)
#error: function 'vec_proxy' not provided by package 'vctrs'
#i Input `rolling_lm` is `lm_roll(adjusted, volume, numeric_date)`.
#i The error occured in group 1: symbol = "AMZN".
X_Plot <-
  FANG %>%
    select(symbol, date, adjusted) %>%
    group_by(symbol) %>%
    mutate(rolling_avg_30 = roll_avg_30(adjusted))


# プロット作成
X_Plot %>%
  pivot_longer(cols = c(adjusted, rolling_avg_30)) %>%
  plot_time_series(date, value, .color_var = name,
                   .facet_ncol = 2, .smooth = FALSE,
                   .interactive = FALSE)




# ***** ローリング回帰 ***************************************************


# Rolling regressions are easy to implement using `.unlist = FALSE`
lm_roll <- slidify(~ lm(..1 ~ ..2 + ..3), .period = 90,
                   .unlist = FALSE, .align = "right")


# データ確認
X_Plot <-
  FANG %>%
    select(symbol, date, adjusted, volume) %>%
    group_by(symbol) %>%
    mutate(numeric_date = as.numeric(date))


# プロット作成
X_Plot %>%
  mutate(rolling_lm = lm_roll(adjusted, volume, numeric_date)) %>%
  filter(!is.na(rolling_lm))


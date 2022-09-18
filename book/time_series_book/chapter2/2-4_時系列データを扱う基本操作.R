# ***********************************************************************************************
# Title   : RとStan出始める心理学のための時系列分析入門
# Chapter : 2 時系列分析の基本操作
# Theme   : 時系列データを扱う基本操作
# Date    : 2022/09/18
# Page    : P41 - P50
# URL     : https://github.com/komorimasashi/time_series_book
# ***********************************************************************************************


# ＜概要＞
# - 時系列処理でよく用いる処理をRで行う
# - 時系列データでは自己共分散と自己相関係数が最も重要な統計量


# ＜目次＞
# 0 準備
# 1 ラグ処理
# 2 階差
# 3 移動平均
# 4 ローリング相関係数
# 5 自己相関


# 0 準備 ---------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(forecast)
library(tsibble)
library(lubridate)
library(zoo)
library(gtrendsR)


# 1 ラグ処理 ----------------------------------------------------------

# 時系列データの作成
x <- 1:120 %>% ts(start = c(2020, 1), frequency = 12)

# ラグ(tsクラス)
# --- 1年前にずらしたデータ
# --- 開始時点が2019年1月に変更されている
x %>% stats::lag(k = 12)

# ラグ(tsクラス以外)
# tsクラス以外のオブジェクトのラグを求めるにはdplyr::lag()を使う
x <- 1:10
x %>% dplyr::lag(k = 1)

# データフレームにおけるラグ処理
data.frame(x = 1:10) %>%
  mutate(lead = lead(x, k = 1)) %>%
  mutate(lag = lag(x, k = 1))


# 2 階差 -------------------------------------------------------------

# 階差
x <- 0:10 %>% cumsum()

# 1階階差
# --- 等差数列になる
x %>% diff(lag = 1, differences = 1)

# 2階階差
# --- 1階差の階差なのですべて1
x %>% diff(lag = 1, differences = 2)


# 3 移動平均 --------------------------------------------------------

# データの作成
# --- 日付
# --- メインデータ
day <- seq(from = as.Date("2020-12-25"), to = as.Date("2021-01-10"), by = "day")
body <- day %>% length() %>% rnorm()

# 移動平均
body_ma <- body %>% rollmean( 7, align = "right", fill = NA)

# データフレームで移動平均計算
df_ma <-
  tibble(day = day) %>%
    mutate(original = body) %>%
    mutate(moving_average = body_ma) %>%
    pivot_longer(-day, names_to = "dat", values_to = "value")

# プロット作成
df_ma %>%
  ggplot(aes(day)) +
  geom_line(aes(y = value, colour = dat))


# 4 ローリング相関係数 ----------------------------------------------------

# 関数定義
# --- 相関係数を求めるcorrelationという名前の関数を作成する
correlation <- function(x) {
  return(cor(x[, 1], x[, 2]))
}

# データフレームの作成
# --- 2つの系列は正規乱数で作成している
df <- tibble(x1 = rnorm(50),
             x2 = rnorm(50))

# ローリング相関係数
# --- 出力値はベクトル
df %>% rollapplyr(width = 20, FUN = correlation, by.column = F)


# 5 自己相関 --------------------------------------------------------

# ホワイトノイズデータ ---------------------------

# ＜ポイント＞
# - ホワイトノイズとは平均0の乱数のデータ系列
#   --- 自己相関はほぼゼロとなる


# データ作成
white.noise <- rnorm(100)

# プロット作成
white.noise %>% ts.plot()

# 自己相関関数
# --- ホワイトノイズの自己相関はほぼゼロ
wn_acf <- white.noise %>% acf()
wn_acf$acf %>% mean()
wn_acf$acf %>% abs() %>% mean()


# ランダムウォークデータ ---------------------------

# ＜ポイント＞
# - ランダムウォークデータとはホワイトノイズを累和で積み上げたデータ系列
#   --- 平均値が時間とともに変化する非定常過程
#   --- 隣り合った系列の差分(階差)は定常過程
#   --- 単位根仮定と呼ばれる時系列の代表


# データ作成
# --- ホワイトノイズを累和したもの
random.walk <- rnorm(100) %>% cumsum()

# プロット作成
random.walk %>% ts.plot()

# 自己相関関数
# --- ランダムウォークは高い自己相関を持つ（累和することで過去の情報を記憶している）
rw_acf <- random.walk %>% acf()
rw_acf$acf %>% mean()


# 季節成分の含まれたデータ ----------------------------------------------

# Google Trendからデータ取得
# --- ｢月曜日｣という言葉の検索回数
trend <- gtrends(keyword = "月曜日", geo = "JP", time = "2020-08-01 2020-12-01")

# データ確認
trend %>% print()
trend %>% names()

# プロット作成
trend %>% plot()

# 自己相関の確認
trend$interest_over_time$hits %>% acf()

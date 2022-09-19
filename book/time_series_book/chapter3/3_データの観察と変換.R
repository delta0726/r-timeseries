# ***********************************************************************************************
# Title   : RとStan出始める心理学のための時系列分析入門
# Chapter : 3 時系列の回帰分析
# Theme   : データ観察と変換
# Date    : 2022/09/19
# Page    : P76 - P83
# URL     : https://github.com/komorimasashi/time_series_book
# ***********************************************************************************************


# ＜概要＞
# - 新型コロナウイルスの新規感染者データで時系列分析のプロセスを確認する


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 対数変換
# 3 単位根検定
# 4 季節調整
# 5 分析アプローチの選択


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(coronavirus)
library(tseries)
library(CADFtest)

# データロード
# --- 新型コロナの感染者データ
data(coronavirus)

# データ確認
coronavirus %>% glimpse()
coronavirus %>% dim()


# 1 データ確認 --------------------------------------------------------------

# ＜ポイント＞
# - データはプロットすることで特徴が見えやすくなる


# データ抽出
covid.Ja <-
  coronavirus %>%
    filter(type == "confirmed") %>%
    filter(date <= as.Date("2021/09/01")) %>%
    filter(country == "Japan")

# プロット作成
covid.Ja %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line() +
  scale_x_date(date_breaks = "3 month", date_labels =  "%Y-%m")  +
  xlab("Day") +
  ylab("Cases")


# 2 対数変換 --------------------------------------------------------------

# ＜ポイント＞
# - 時系列データはy軸を対数変換することで特性が見えやすくなることがある
#   --- 指数的に水準が増えていく系列に有効


# プロット作成
# --- ggplotのオプションで対数変換
covid.Ja %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line() +
  scale_y_log10() +
  scale_x_date(date_breaks = "3 month", date_labels =  "%Y-%m")  +
  xlab("Day") +
  ylab("Cases")

# プロット作成
# --- 2020年2月10日以降
covid.Ja %>%
  filter(date >= as.Date("2020/02/10")) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_line() +
  scale_y_log10() +
  scale_x_date(date_breaks = "3 month", date_labels =  "%Y-%m")  +
  xlab("Day") +
  ylab("Cases")


# 3 単位根検定 --------------------------------------------------------------

# case系列の取得
# --- 対数変換
logcases <-
  covid.Ja %>%
    filter(date >= as.Date("2020/02/10")) %>%
    pull(cases) %>%
    log()

# プロット作成
logcases %>% ts.plot()
logcases %>% diff() %>% ts.plot()

# ADF検定（原系列）
# --- トレンド項も定数項もあり
logcases %>%
  CADFtest(type = 'trend', max.lag.y = 5, criterion = 'AIC')

# ADF検定（差分系列）
# --- トレンド項も定数項もあり
logcases %>%
  diff() %>%
  CADFtest(type = 'trend', max.lag.y = 5, criterion = 'AIC')


# 4 季節調整 -------------------------------------------------------

# ＜ポイント＞
# - 新型コロナウイルスの新規感染者数は日曜日と月曜日に少ない傾向がある
#   ---


# STL分解
# --- 季節成分とトレンドとそれ以外に分解する
logcases.diff.stl <-
  logcases %>%
    diff() %>%
    ts(frequency = 7) %>%
    stl(s.window = "periodic")

# データ確認
logcases.diff.stl %>% names()
logcases.diff.stl %>% use_series(time.series) %>% head()

# プロット作成
logcases.diff.stl %>% plot()

# 季節調整後のデータ
# --- TrendとRemainderのみで構成
logcases.diff.stl %$%
  (time.series[, 2] + time.series[, 3]) %>%
  plot()


# 5 分析アプローチの選択 ----------------------------------------------------

# ＜欠損値の扱い＞
# - ARIMA法や反復測定分散分析などは欠損値があると基本的に分析できない
# - 欠損値は安易に補完せず、状態空間モデルなど欠損値があっても分析できる手法を検討

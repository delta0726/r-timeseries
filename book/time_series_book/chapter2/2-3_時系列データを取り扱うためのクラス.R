# ***********************************************************************************************
# Title   : RとStan出始める心理学のための時系列分析入門
# Chapter : 2 時系列分析の基本操作
# Theme   : 時系列データを取り扱うためのクラス
# Date    : 2022/09/18
# Page    : P35 - P41
# URL     : https://github.com/komorimasashi/time_series_book
# ***********************************************************************************************


# ＜概要＞
# - 時系列データを扱うクラスとしてtsとtsibbleを確認する


# ＜目次＞
# 0 準備
# 1 tsクラス
# 2 tsibbleで時系列を整然データとして扱う


# 0 準備 ---------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(forecast)
library(tsibble)
library(lubridate)


# 1 tsクラス -----------------------------------------------------------

# データロード
data(UKgas)

# クラス確認
UKgas %>% class()

# プロット
UKgas %>% ts.plot()
UKgas %>% autoplot()

# 開始日と終了日の（年，四半期）がベクトルとして格納
UKgas %>% start()
UKgas %>% end()

# tsオブジェクトの作成
# --- 2020年1月から60ヵ月分の観測値の仮想データ（ランダムウォーク）
set.seed(9999)
data <- rnorm(n = 60) %>% cumsum()
x <- ts(data = data, start = c(2020, 1), frequency = 12)
x %>% ts.plot()

# データの一部抽出
x %>% window(c(2020, 1), c(2021, 3))


# 2 tsibbleで時系列を整然データとして扱う --------------------------------

# ＜ポイント＞
# - tsibbleはtsクラスをtibble形式に変換したデータセット
#   --- モダンフレームワークが使える


# tsクラスをtsibbleに変換
UKgas %>% as_tsibble()
UKgas %>% as_tsibble() %>% class()

# プロット作成
UKgas %>%
  as_tsibble() %>%
  ggplot() +
  geom_line(aes(x = index, y = value))

# プロット作成
# --- UKgasの年集計値
# --- index_by()は{dplyr}のgroup_by()の時系列版です
UKgas %>%
  as_tsibble() %>%
  index_by(Year = ~ year(.)) %>%
  summarise(Mean = mean(value)) %>%
  ggplot() +
  geom_line(aes(x = Year, y = Mean))

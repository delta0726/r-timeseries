# Title     : What is the Time Series Signature?
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/4
# URL       : https://business-science.github.io/timetk/articles/TK01_Working_With_Time_Series_Index.html





# ＜ポイント＞
# - Time Series Signatureとは、日付/時間データを最大限活用するために日付属性を変換する仕組みを





# 1.準備 -----------------------------------------------------------------------


library(tidyverse)
library(tidyquant)
library(timetk)


# データ準備
data("FANG")


# データ確認
FANG %>% print()
FANG %>% glimpse()


# FBのみ抽出
# --- 日次の株価データ
FB_vol_date <-
  FANG %>%
    filter(symbol == "FB") %>%
    select(date, volume)


# 確認
FB_tbl %>% print()



# 2.Time Series Index --------------------------------------------------------------

# ＜ポイント＞
# - Time Series Signatureでインデックスを分析する前にデータセットから日付データを取得する必要がある
# - tk_index()はデータセットから日付データを抽出する
#   --- xts/ts/zooなどの日付オブジェクトは予め日付データが明示されている
#   --- データフレームの場合、


# 日付インデックスの取得
# --- timetk_idx = FALSE
# --- データフレームから日付データを探してきて出力
idx_date <- FB_vol_date %>% tk_index(timetk_idx = FALSE)
idx_date %>% str()



# 3.Time Series Signature ------------------------------------

# ＜ポイント＞
# - 時系列データからプロパティ情報を得る
# - 日付ベクトルに適用すると、日付に属性に付加される形でデータフレームが作成される
# - データフレームに適用すると、元の列に日付属性が追加される

# 日付データ
idx_date %>% glimpse()
idx_date %>% length()


# 日付ベクトルから日付属性を取得
# --- tk_get_timeseries_signature()
idx_signature <- idx_date %>% tk_get_timeseries_signature()
idx_signature %>% print()
idx_signature %>% glimpse()


# データフレームから日付属性を取得
# --- tk_augment_timeseries_signature()
FB_vol_date_signature <- FB_vol_date %>% tk_augment_timeseries_signature(.date_var = date)
FB_vol_date_signature




# Example Benefit 2: Modeling is easier
fit <- lm(volume ~ year + month.lbl, data = FB_vol_date_signature)
summary(fit)



# 4.日付属性の使いどころ ------------------------------------

# ＜ポイント＞
# - 日付データを用いたモデリングを行うことができる


# データ確認
FB_vol_date_signature %>% print()


# 日付データを説明変数として回帰
fit <- lm(volume ~ year + month.lbl, data = FB_vol_date_signature)
fit %>% summary()



# 5.Time Series Summary --------------------------------------------------------------

# ＜ポイント＞
# - 日付サマリーを取得する

# サマリー内容
# --- 前半の6データは一般的なサマリー
idx_date %>% tk_get_timeseries_summary() %>% .[,1:6]


# サマリー内容
# --- 後半の6データはdiffのサマリー
# --- データ頻度がどのようになっているかを示唆する
tk_get_timeseries_summary(idx_date)[,7:12]


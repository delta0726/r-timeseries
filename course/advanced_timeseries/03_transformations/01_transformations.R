# ****************************************************************************
# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: ADVANCED TIME SERIES FORECASTING FOR BUSINESS
# MODULE: TIME SERIES TRANSFORMATIONS -----
# ****************************************************************************


# GOAL ----
# - timetkのベクトル変換を扱う関数群を学ぶ

# OBJECTIVES ----
# - Variance Reduction - Log, Log1P, Box Cox
# - Rolling & Smoothing
# - Range Reduction - Normalization & Standardization
# - Imputation & Outliers
# - Lags & Differencing
# - Fourier Series（フーリエ級数）
# - Confined Interval Forecasting



# ＜目次＞
# 0 準備
# 1 バリアンスの削減
# 2 ローリングとスムージング
# 3 スケーリング
# 4 欠損値処理と異常値処理
# 5 ラグ処理と差分変換
# 6 フーリエ系列
# 7 データ変換とモデリング


# 0 準備 ------------------------------------------------------------------------

library(tidyverse)
library(timetk)
library(lubridate)


# データ1
google_analytics_summary_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")
google_analytics_summary_tbl %>% print()

# データ2
mailchimp_users_tbl <- read_rds("00_data/mailchimp_users.rds")
mailchimp_users_tbl %>% print()

# データ3
transactions_tbl <- read_rds("00_data/transactions_weekly.rds")
transactions_tbl %>% print()


# * データ準備 ----------------------------------------------

# データ加工
# --- 複数系列の日次データ
google_analytics_summary_long_tbl <-
  google_analytics_summary_tbl %>%
    mutate(date = ymd_h(dateHour)) %>%
    select(-dateHour) %>%
    pivot_longer(-date) %>%
    group_by(name)

# データ加工
# --- 1系列の日次データ
subscribers_daily_tbl <-
  mailchimp_users_tbl %>%
    summarise_by_time(optin_time, .by = "day", optins = n()) %>%
    pad_by_time(.pad_value = 0, .start_date = "2018-06-01")


# * データ確認 ----------------------------------------------

# プロット作成
# --- 複数系列の日次データ
google_analytics_summary_long_tbl %>%
  group_by(name) %>%
  plot_time_series(.date_var = date, .value = value)

# プロット作成
# --- 1系列の日次データ
subscribers_daily_tbl %>%
  plot_time_series(.date_var = optin_time, .value = optins)


# 1 バリアンスの削減 --------------------------------------------------------

# ＜ポイント＞
# - 定常過程は分散も時間によらず一定であることを想定する
#   --- 時系列において分散は時期によって変わるのが一般的
#   --- 分散安定化の変換が必要（対数変換 Bos-Cox変換 Yao-Johnson変換）

# ＜参考＞
# - Rによる時系列分析入門（P296）

# ＜サブタイトル＞
# * はじめに
# * 対数変換
# * Box-Cox変換


# * はじめに ----------------------------------------

# 元データの確認
# --- 2018-11-19などにスパイクがいくつか存在するようだ
subscribers_daily_tbl %>%
  plot_time_series(.date_var = optin_time, .value = optins)

# スパイクの確認
# --- 時系列の線形回帰を使って平均的な水準を確認
# --- Adjusted R-squared:  0.01342
subscribers_daily_tbl %>%
  plot_time_series_regression(
      .date_var = optin_time,
      .formula = optins ~ as.numeric(optin_time) +
                 wday(optin_time, label = TRUE) +
                 month(optin_time, label = TRUE),
      .show_summary = TRUE
  )


# * 対数変換 ----------------------------------------

# ＜ポイント＞
# - 対数変換すると大きな値が縮尺されて見やすくなる
# - 対数変換は1以上の値でないとできないため、1以上となるようにシフトして計算する


# 対数変換（エラー発生）
# --- 0-1の値は対数変換できない
# --- log(0)=-Inf
# --- log(1)=0
subscribers_daily_tbl %>%
  plot_time_series(.date_var = optin_time, .value = log(optins))

# 対数変換
# --- 1が下限になるようにデータをシフトして対数変換
subscribers_daily_tbl %>%
  plot_time_series(.date_var = optin_time, .value = log1p(optins))

# 元系列に再変換
# --- 対数変換 ⇒ exp変換
subscribers_daily_tbl %>%
  plot_time_series(.date_var = optin_time, .value = expm1(log1p(optins)))

# スパイクを評価
# --- 時系列回帰を用いて評価
# --- R2で見るとフィッティングが改善している（Adjusted R-squared:  0.4829）
# --- 説明変数も統計的に有意なものが出てきた
subscribers_daily_tbl %>%
  plot_time_series_regression(
      .date_var = optin_time,
      .formula = log1p(optins) ~ as.numeric(optin_time) +
                 wday(optin_time, label = TRUE) +
                 month(optin_time, label = TRUE),
      .show_summary = TRUE
  )

# グループデータへの適用
# --- 対数変換
google_analytics_summary_long_tbl %>%
  plot_time_series(.date_var = date, .value = log1p(value), .color_var = name)


# * Box-Cox変換 ----------------------------------------------------

# ＜ポイント＞
# - Box-Cox変換は対数変換を一般化したもの
#   --- lambda引数で対数変換の適用度合いをコントロール（チューニングが必要）
#   --- Auto Arimaなどでは自動的に適用される（非常に効果的なため）
# - Box-Cox変換も0-1やマイナス値を変換できない点は同様
#   --- Yao-Johnson変換

# Box-Cox変換
# --- lambdaは正規化するように自動チューニング（任意の値を指定することも可能）
# --- 元データは1以上の値である必要がある（対数変換と同様）
subscribers_daily_tbl %>%
  plot_time_series(.date_var = optin_time,
                   .value = box_cox_vec(optins + 1, lambda = "auto"))

# スパイクの確認
# --- 時系列回帰を用いて評価
# --- R2で見るとフィッティングが改善している（Adjusted R-squared:  0.5279 ）
# --- 説明変数も統計的に有意なものが出てきた
subscribers_daily_tbl %>%
  plot_time_series_regression(
      .date_var = optin_time,
      .formula  = box_cox_vec(optins + 1) ~ as.numeric(optin_time) +
                  wday(optin_time, label = TRUE) +
                  month(optin_time, label = TRUE),
      .show_summary = TRUE
  )


# プロット作成(Box-Cox変換 ⇒ Box-Cox逆変換)
subscribers_daily_tbl %>%
    plot_time_series(.date_var = optin_time,
                     .value = box_cox_vec(optins + 1, lambda = -0.2) %>% box_cox_inv_vec(lambda = -0.2))

# グループデータへの適用
# --- Box-Cox変換
# --- グループ化を適用済のデータ
google_analytics_summary_long_tbl %>%
  plot_time_series(.date_var = date, .value = box_cox_vec(value))


# * 計算証明 -------------------------------------------

# Box-Cox変換を元データに再変換
# --- box_cox_vec()でlambdaを自動チューニングして変換
# --- box_cox_inv_vec()で元のデータに逆変換（チューニング時のlambdaを使用）
# --- 結果として、元の系列のプロットが表示される
google_analytics_summary_long_tbl %>%
    mutate(value_trans = box_cox_vec(value)) %>%
    group_split() %>%
    
    map2(.y = 1:3, .f = function(df, idx) {
        if (idx == 1) lambda <- 0.441313194936969
        if (idx == 2) lambda <- -0.0023793550944814
        if (idx == 3) lambda <- -0.116626712183629
        
        df %>%
            mutate(value_trans_inv = box_cox_inv_vec(value_trans, lambda = lambda))
    }) %>%
    bind_rows() %>%
    
    group_by(name) %>%
    plot_time_series(date, value_trans_inv)



# 2 ローリングとスムージング ------------------------------------------------------------


# ＜ポイント＞
# - ローリングによるスムージング変換には以下の効果がある
#   --- 時系列データのトレンドを視覚的に把握することができる
#   --- 予測精度を改善することができるケースがある
#   --- 異常値削減/トレンド追跡に効果がある
# - 実務では移動平均を使うケースが多い（過去への依存が変化に遅れを生む）
# - 以降ではローリング計算を一般化するslidify()を学ぶ

# ＜参考＞
# - Rによる時系列分析入門（P89-121）

# ＜サブタイトル＞
# * Sliding / Rolling Functions
# * LOESSスムージング
# * ローリング相関係数
# * 移動平均による予測の問題点


# * Sliding / Rolling Functions ------------------------------------

# ＜ポイント＞
# - slidify_vec()を用いてローリング計算を一般化する
#   --- 1変数のローリング計算を想定


# ローリング計算（ベクトル単位）
# --- rolling Minimum
# --- .align引数は対象データの抽出方法を定義
#      - ここでは真ん中のデータを中心に3つの要素を抽出してローリング計算
# --- .partial引数をTRUEにすると端のデータの場合に確保できる要素数で計算
1:10
1:10 %>% slidify_vec(.f = min, .period = 3, .align = "center", .partial = TRUE)

# ローリング計算（グループレベル）
# --- グループ化済のデータフレームを使用
# --- ローリング平均(7day * 24hours)
google_analytics_summary_long_tbl %>%
  mutate(value_roll = slidify_vec(value,
                                  .f = mean,
                                  .period = 24 * 7,
                                  .align = "center",
                                  .partial = TRUE)) %>%
  pivot_longer(contains("value"), names_repair = "unique") %>%
  rename(names = `name...2`, names_2 = `name...3`) %>%
  group_by(names) %>%
  plot_time_series(.date_var = date, .value = value, .color_var = names_2, .smooth = FALSE)


# * LOESSスムージング --------------------------------------------

# ＜ポイント＞
# - smooth_vec()を用いてLOESS変換を行う


# プロット作成
# --- グループデータを使用（グループ化済）
# --- ローリング平均(7day * 24hours)
google_analytics_summary_long_tbl %>%
  mutate(value_smooth = smooth_vec(value,
                                   period = 24 * 7,
                                   # span = 0.75
                                   degree = 0)) %>%
  pivot_longer(contains("value"), names_repair = "unique") %>%
  rename(names = `name...2`, names_2 = `name...3`) %>%
  group_by(names) %>%
  plot_time_series(date, value, .color_var = names_2, .smooth = FALSE)


# * ローリング相関係数 ----------------------------------------

# ＜ポイント＞
# - slidify()を用いて列単位で計算する
# - 複数変数のローリング計算を想定（例：相関係数）
# - tibbletime::rollify()と同じ


# 相関係数
cor(1:10, seq(0, 20, length.out = 10))

# 関数定義
# --- ローリング相関係数
rolling_cor_24_7 <-
  slidify(.f = ~ cor(.x, .y, use = "pairwise.complete.obs"),
          .period  = 24*7,
          .align   = "center",
          .partial = FALSE)

# ローリング相関係数
# --- 最後のプロットにローリング相関係数が表示される
google_analytics_summary_tbl %>%
  mutate(rolling_cor_pageviews_organic = rolling_cor_24_7(pageViews, organicSearches)) %>%
  mutate(dateHour = ymd_h(dateHour)) %>%
  select(-sessions) %>%
  pivot_longer(-dateHour) %>%
  group_by(name) %>%
  plot_time_series(dateHour, value)



# * 移動平均による予測の問題点 ------------------------------------

# ＜ポイント＞
# - ローリング平均は基本的に遅れる


# データ作成
# --- ローリング平均(8日間)
X_Plot <-
  transactions_tbl %>%
    mutate(mavg_8 = slidify_vec(revenue, .f = ~ mean(.x, na.rm = TRUE),
                                .period = 8, .align = "right")) %>%
    bind_rows(future_frame(., .length_out = 8)) %>%
    fill(mavg_8, .direction = "down")

# データ確認
X_Plot %>% print(n = nrow(.))

# プロット作成
# --- 移動平均が遅行することを確認
X_Plot %>%
  pivot_longer(-purchased_at) %>%
  plot_time_series(purchased_at, value, name, .smooth = F)


# 3 スケーリング --------------------------------------------------------------------

# ＜ポイント＞
# - 複数系列を比較/合成する際に必要な操作

# ＜サブタイトル＞
# * リスケーリング（between 0 to 1）
# * データ基準化（mean：0 sd:1）


# * リスケーリング（between 0 to 1） ---------------------------------------------

# ＜ポイント＞
# - Normalizeは0-1の間にスケーリングすることを指す
#   --- 特に時系列を意識したものではないので一般的に使える
# - normalize_vec()は{dplyr}で列単位でベクトル計算することを想定
# - recipes::step_range()で前処理の1つとして一般化されている

# プロット作成
# --- 0-1に基準化
google_analytics_summary_long_tbl %>%
  mutate(value = normalize_vec(value)) %>%
  ungroup() %>%
  plot_time_series(date, value, name)


# * データ基準化（mean：0 sd:1） ---------------------

# ＜ポイント＞
# - Normalizeは平均0/標準偏差1にスケーリングすることを指す
#   --- 特に時系列を意識したものではないので一般的に使える
# - normalize_vec()は{dplyr}で列単位でベクトル計算することを想定
# - recipes::step_normalize()で前処理の1つとして一般化されている

# プロット作成
# --- Zスコア化
google_analytics_summary_long_tbl %>%
  mutate(value = standardize_vec(value)) %>%
  ungroup() %>%
  plot_time_series(date, value, name)


# 4 欠損値処理と異常値処理 ------------------------------------------------------

# ＜ポイント＞
# - 複数系列を比較/合成する際に必要な操作

# ＜サブタイトル＞
# * 欠損値補完
# * データクリーニング (欠損値処理＋異常値除去)
# * 外れ値の影響


# * 欠損値補完 --------------------------------------------------------

# ＜ポイント＞
# - ts_impute_vec()はデータ系列全体からNAの箇所を予測する
#   --- 直前の値から推定するのではないので、期初のNAも補完することができる
#   --- forecast::na.interp()のラッパー関数


# 欠損値確認
# --- ゼロをNAに置換(元々NAを0としていた)
subscribers_daily_tbl_in_na <-
  subscribers_daily_tbl %>%
    mutate(optins_na = ifelse(optins == 0, NA, optins))

# 欠損値補完
# --- ｢optins｣と｢optins_imputed｣を比較（Plotlyの系列を切替）
# --- 不自然ではあるが、水準感や規則性を元系列から推定して欠損値が補完される
subscribers_daily_tbl_in_na %>%
  mutate(optins_imputed = ts_impute_vec(optins_na, period = 7)) %>%
  pivot_longer(-optin_time) %>%
  plot_time_series(optin_time, log1p(value), .color_var = name, .smooth = F)



# * データクリーニング (欠損値処理＋異常値除去) --------------------------------

# ＜ポイント＞
# - ts_clean_vec()は欠損値処理と異常値処理を同時に行う
#   --- forecast::tsclean()のラッパー関数
#   --- 内部的には季節性分解などを行っている

# アノマリー確認
subscribers_daily_tbl %>%
  plot_anomaly_diagnostics(.date_var = optin_time, .value = optins)

# データクリーニング
# --- ゼロをNAに置換(元々NAを0としていた)
subscribers_cleaned_daily_tbl <-
  subscribers_daily_tbl %>%
    mutate(optins_na = ifelse(optins == 0, NA, optins)) %>%
    mutate(optins_cleaned = ts_clean_vec(optins_na, period = 7))

# クリーニング結果の確認
# --- バリアンス削減なし
subscribers_cleaned_daily_tbl %>%
  pivot_longer(-optin_time) %>%
  plot_time_series(optin_time, value, name, .smooth = F)

# クリーニング結果の確認
# --- バリアンス削減あり
subscribers_cleaned_daily_tbl %>%
  pivot_longer(-optin_time) %>%
  plot_time_series(optin_time, log1p(value), name, .smooth = F)


# * 外れ値の影響 ---------------------------------------------------------

# ＜ポイント＞
# - 外れ値を除去することでフィッティングがよくなる
# - 時系列回帰を用いて異常値を把握


# スパイクを含むデータ
# --- 時系列回帰でスパイク度合いを確認
# --- Adjusted R-squared:  0.01342
subscribers_cleaned_daily_tbl %>%
  plot_time_series_regression(
      optin_time,
      .formula = optins ~ as.numeric(optin_time) +
          wday(optin_time, label = TRUE) +
          month(optin_time, label = TRUE),
      .show_summary = TRUE
  )

# バリアンス削減
# --- log1p()で異常値処理
# --- Adjusted R-squared:  0.4829
subscribers_cleaned_daily_tbl %>%
  plot_time_series_regression(
      optin_time,
      .formula = log1p(optins) ~ as.numeric(optin_time) +
          wday(optin_time, label = TRUE) +
          month(optin_time, label = TRUE),
      .show_summary = TRUE
  )

# クリーニング
# --- ts_clean_vec()でクリーニング
# --- Adjusted R-squared:  0.439
subscribers_cleaned_daily_tbl %>%
  plot_time_series_regression(
      optin_time,
      .formula = optins_cleaned ~ as.numeric(optin_time) +
          wday(optin_time, label = TRUE) +
          month(optin_time, label = TRUE),
      .show_summary = TRUE
  )


# 5 ラグ処理と差分変換 ---------------------------------------------------------------------

# ＜ポイント＞
# - ラグ処理や差分処理は時系列モデリングで多用される

# ＜サブタイトル＞
# * ラグ処理
# * 差分処理
# * 複数グループで差分系列を比較


# * ラグ処理 -----------------------------------------------------

# ＜ポイント＞
# - 時系列の特徴量エンジニアリングでよく用いられる
# - 自己相関の存在に着目した考え方


# ラグ系列の追加
# --- timetk::lag_vec()
subscribers_daily_tbl %>%
  mutate(optins_lag_1 = lag_vec(optins, lag = 2))

# ラグ系列の追加
# --- dplyr::lag()
subscribers_daily_tbl %>%
  mutate(optins_lag_1 = lag(optins, n = 2))

# ACFプロット
# --- 自己相関の診断
# --- ラグ処理は自己相関の議論とセット（ACFプロットで診断）
subscribers_daily_tbl %>%
  plot_acf_diagnostics(optin_time, log1p(optins))


# 原系列と時系列回帰の比較
# --- tk_augment_lags()で複数のラグ系列を同時作成
# --- ラグ系列から時系列回帰を作成（ARモデル）
# --- Adjusted R-squared:  0.5319
subscribers_daily_tbl %>%
  tk_augment_lags(.value = optins, .lags = c(1, 2, 6, 14)) %>%
  drop_na() %>%
  plot_time_series_regression(
      optin_time,
      .formula = log1p(optins) ~ log1p(optins_lag1) + log1p(optins_lag2) +
          log1p(optins_lag6) + log1p(optins_lag14),
      .show_summary = TRUE
  )


# * 差分処理 --------------------------------------------------

# ＜ポイント＞
# - 定常過程を抽出するテクニックの1つ
#   --- 差分が定常過程に従うケースは多い


# 累和グラフ
subscribers_daily_tbl %>%
  mutate(total_optins = cumsum(optins)) %>%
  plot_time_series(.date_var = optin_time, .value = total_optins, .smooth = FALSE)

# 差分系列のグラフ
# --- 原系列
# --- 累和
# --- Diff1: 速度
# --- Diff2: 加速度
subscribers_daily_tbl %>%
  mutate(total_optins = cumsum(optins),
         optins_diff_1 = diff_vec(total_optins, lag = 1, difference = 1),
         optins_diff_2 = diff_vec(total_optins, lag = 1, difference = 2)) %>%
  pivot_longer(-optin_time) %>%
  group_by(name) %>%
  plot_time_series(optin_time, value, name, .smooth = FALSE)


# * 複数グループで差分系列を比較 --------------------------------

# 原系列を差分系列に変換
# diff_vec(): Initial values: 79
# diff_vec(): Initial values: 16
# diff_vec(): Initial values: 47
google_analytics_summary_diff_tbl <-
  google_analytics_summary_tbl %>%
    mutate(across(pageViews:sessions, .fns = diff_vec)) 

# 確認
google_analytics_summary_tbl %>% print()
google_analytics_summary_diff_tbl %>% print()

# プロット作成
google_analytics_summary_diff_tbl %>%
  mutate(dateHour = ymd_h(dateHour)) %>%
  pivot_longer(-dateHour) %>%
  plot_time_series(dateHour, value, name, .smooth = FALSE)


# 差分系列を元に戻す
google_analytics_summary_diff_tbl %>%
  mutate(pageViews_diff_inv = diff_inv_vec(pageViews, initial_values = 79))


# 確認用
google_analytics_summary_tbl %>% print()




# 6 フーリエ系列 -----------------------------------------------------------------------

# ＜ポイント＞
# - 季節性や自己相関を組み込む際に役立つテクニック
# - BENEFIT: Don't need a lag, just need a frequency (based on your time index)

# ＜サブタイトル＞
# * フーリエ系列の作成
# * 複数のフーリエ系列の作成


# * フーリエ系列の作成 --------------------------------------------

# ＜ポイント＞
# - timetk::fourier_vec()でフーリエ系列を追加することができる


# プロット作成
# --- sin波
# --- 原系列のフーリエ系列
subscribers_daily_tbl %>%
  mutate(sin14_k1 = fourier_vec(optin_time, period = 14, K = 1, type = "sin")) %>%
  plot_time_series(optin_time, sin14_k1, .smooth = FALSE)


# プロット作成
# --- sin波とcos波
# --- 原系列のフーリエ系列
subscribers_daily_tbl %>%
  mutate(sin14_k1 = fourier_vec(optin_time, period = 14, K = 1, type = "sin")) %>%
  mutate(cos14_k1 = fourier_vec(optin_time, period = 14, K = 1, type = "cos")) %>%
  select(-optins) %>%
  pivot_longer(matches("(cos)|(sin)")) %>%
  plot_time_series(optin_time, value, name, .smooth = FALSE)


# * 複数のフーリエ系列の作成 -------------------------------------------

# プロット作成
# --- 周期に合わせて複数系列を作成（2W / 1M / 3M / 1Y）
# --- 時系列回帰にフーリエ系列を追加して予測
# --- Adjusted R-squared:  0.4719
subscribers_daily_tbl %>%
  tk_augment_fourier(optin_time, .periods = c(14, 30, 90, 365), .K = 2) %>%
  plot_time_series_regression(
      optin_time,
      .formula = log1p(optins) ~ as.numeric(optin_time) + . - optin_time,
      .show_summary = TRUE
  )



# 7 データ変換とモデリング ----------------------------------------------------

# ＜サブタイトル＞
# * 準備：スケール対数変換
# * 準備：時系列データへの適用
# * モデル用のデータ変換
# * モデリング
# * 将来データフレームの作成
# * 予測データの作成
# * データ結合
# * 元のスケールに変換


# * 準備：スケール対数変換 -------------------------------

# ＜ポイント＞
# - 上下限付きの対数変換：log_interval_vec()
# - スケーリングしてから対数変換（スケール対数変換）


# 数値ベクトル
1:10

# 上下限でスケーリングして対数変換
values_transformed_vec <-
  1:10 %>%
    log_interval_vec(limit_lower = 0, limit_upper = 15)

# 確認
values_transformed_vec %>% print()

# 対数変換を元に戻す
values_transformed_vec %>%
  log_interval_inv_vec(limit_lower = 0, limit_upper = 15)

# データ作成
# --- プロット用データ
new_values_transformed_vec <- c(values_transformed_vec, c(.75, 1.5, 2.0))

# プロット作成
# --- スケーリングしてから対数変換
new_values_transformed_vec %>% plot()

# プロット作成
# --- 元のスケールに戻す
new_values_transformed_vec %>%
  log_interval_inv_vec(0, 15) %>%
  plot()


# * 準備：時系列データへの適用 -------------------------------

# スケール対数変換の設定
limit_lower <- 0
limit_upper <- 3650.8
offset      <- 1

# プロット作成
# --- スケール対数変換を適用
subscribers_daily_tbl %>%
  plot_time_series(optin_time, log_interval_vec(optins,
                                                limit_lower = limit_lower,
                                                limit_upper = limit_upper,
                                                offset      = offset))


# * モデル用のデータ変換 -----------------------------------

# 設定：スケール対数変換
limit_lower <- 0
limit_upper <- 3650.8
offset      <- 1

# 設定：フーリエ変換
fourier_periods <- c(6, 14, 30, 90, 365)
fourier_order   <- 5

# データ作成
# --- スケール対数変換
# --- フーリエ変換
data_transformed_tbl <-
  subscribers_daily_tbl %>%
    mutate(optins_trans = log_interval_vec(optins, 
                                           limit_lower = limit_lower, 
                                           limit_upper = limit_upper, 
                                           offset = offset)) %>%
    tk_augment_fourier(optin_time, .periods = fourier_periods, .K = fourier_order) %>%
    select(-optins)


# データ確認
# --- 52列
data_transformed_tbl %>% glimpse()


# * モデリング --------------------------------------------------------

# ＜ポイント＞
# - ｢モデル用のデータ変換｣のデータを元にしている
# - 線形モデルで時系列回帰


# フォーミュラ定義
model_formula <-
  as.formula(optins_trans ~ as.numeric(optin_time) +
                            wday(optin_time, label = TRUE) +
                            month(optin_time, label = TRUE) + . - optin_time)

# 確認
model_formula %>% print()

# プロット作成
# --- 定義したモデルから時系列回帰を作成
data_transformed_tbl %>%
  plot_time_series_regression(.date_var = optin_time,
                              .formula  = model_formula,
                              .show_summary = TRUE)


# 線形回帰モデルの確認
model_fit_lm <- lm(formula = model_formula, data = data_transformed_tbl)
model_fit_lm %>% summary()



# * 将来データフレームの作成 ------------------------------------

# ＜ポイント＞
# - ｢モデル用のデータ変換｣のデータを元にしている


# データ確認
# --- 2020-03-02まで
data_transformed_tbl %>% tail()

# 将来データでフーリエ系列を作成
# --- 2020-03-03から
future_tbl <-
  data_transformed_tbl %>%
    future_frame(.length_out = "6 months") %>%
    tk_augment_fourier(optin_time, .periods = fourier_periods, .K = fourier_order)

# 確認
future_tbl %>% print()
future_tbl %>% select(-optin_time) %>% select(1) %>%  plot()


# * 予測データの作成 ------------------------------------

# 予測
predictions <- model_fit_lm %>% predict(newdata = future_tbl) %>% as.vector()

# 確認
predictions %>% print()


# * データ結合 ----------------------------------------

# 設定情報
conf_interval <- 0.95

# 線形回帰モデルの残差
residuals <- model_fit_lm$residuals %>% as.vector()

# アルファの設定
alpha <- (1 - conf_interval) / 2
1 - alpha

# 信頼区間の設定
# --- Z分布を想定
qnorm(alpha)
qnorm(1-alpha)
abs_margin_error <- abs(qnorm(alpha) * sd(residuals))

# データ作成
forecast_tbl <-
  data_transformed_tbl %>%
    select(optin_time, optins_trans) %>%
    add_column(type = "actual") %>%
    bind_rows(
        future_tbl %>%
            select(optin_time) %>%
            mutate(optins_trans = predictions,
                   type        = "prediction") %>%
            mutate(conf_lo = optins_trans - abs_margin_error,
                   conf_hi = optins_trans + abs_margin_error)
    )

# 確認
forecast_tbl %>% print()

# プロット作成
# --- 予測データと信頼区間
forecast_tbl %>%
  pivot_longer(cols = c(optins_trans, conf_lo, conf_hi)) %>%
  plot_time_series(optin_time, value, .color_var = name, .smooth = FALSE)


# * 元のスケールに変換 ----------------------------------------

# プロット作成
# --- スケール対数変換を解除
# --- 一括処理するためにロング型に変換
forecast_tbl %>%
  pivot_longer(cols = c(optins_trans, conf_lo, conf_hi)) %>%
  plot_time_series(.date_var = optin_time,
                   .value = log_interval_inv_vec(x = value,
                                                 limit_lower = limit_lower,
                                                 limit_upper = limit_upper,
                                                 offset      = offset),
                   .color_var = name,
                   .smooth = FALSE)

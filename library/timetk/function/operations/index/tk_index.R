# Title     : tk_index
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/4
# URL       : https://business-science.github.io/timetk/reference/tk_index.html



# ＜ポイント＞
# - tk_index()は、さまざまな時系列オブジェクト/モデル/予測から日付または日時インデックスを抽出する
#   --- tbl、xts、zoo、zooreg、およびtsオブジェクトに使用することができる
#   --- Arima、ets、およびHoltWintersクラスなどの関数オブジェクトからも日付抽出ができる



# ＜構文＞
# tk_index(data, timetk_idx = FALSE, silent = FALSE)
#
# has_timetk_idx(data)



# 1.準備 --------------------------------------------------------------

library(tidyverse)
library(timetk)


# 時系列データセットの作成
data_tbl <-
  tibble(date = seq.Date(from = as.Date("2000-01-01"), by = 1, length.out = 5),
         x    = rnorm(5) * 10,
         y    = 5:1)


# 確認
data_tbl %>% print()
data_tbl %>% glimpse()


#
data_tbl %>% has_timetk_idx()




# 2.データフレームからの抽出 ------------------------------------------------






#
data_tbl %>% tk_index()




tk_index(data_tbl) # Returns time-based index vector#> [1] "2000-01-01" "2000-01-02" "2000-01-03" "2000-01-04" "2000-01-05"
# Coerce to ts using tk_ts(): Preserves time-basis
data_ts <- ata_tbl %>% tk_ts(d)#> Warning: Non-numeric columns being dropped: datetk_index(data_ts, timetk_idx = FALSE) # Returns regularized index#> [1] 1 2 3 4 5tk_index(data_ts, timetk_idx = TRUE)  # Returns original time-based index vector#> [1] "2000-01-01" "2000-01-02" "2000-01-03" "2000-01-04" "2000-01-05"
# Coercing back to tbl
data_ts %>% tk_tbl(timetk_idx = FALSE) # Returns regularized tbl#> Warning: Warning: No index to preserve. Object otherwise converted to tibble successfully.#> # A tibble: 5 x 2
#>        x     y
#>    <dbl> <dbl>
#> 1  14.1      5
#> 2  -7.95     4
#> 3 -15.7      3
#> 4 -10.4      2
#> 5  10.2      1tk_tbl(data_ts, timetk_idx = TRUE)  # Returns time-based tbl#> # A tibble: 5 x 3
#>   index           x     y
#>   <date>      <dbl> <dbl>
#> 1 2000-01-01  14.1      5
#> 2 2000-01-02  -7.95     4
#> 3 2000-01-03 -15.7      3
#> 4 2000-01-04 -10.4      2
#> 5 2000-01-05  10.2      1



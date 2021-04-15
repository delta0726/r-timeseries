# Title     : box_cox_vec
# Objective : TODO
# Created by: Owner
# Created on: 2020/9/5
# URL       : https://business-science.github.io/timetk/reference/box_cox_vec.html



# ＜ポイント＞
# - {forecast}が提供するBox-Cox変換のラッパー関数
# - 自動でlambdaを計算する機能を備えている




# ＜構文＞
# box_cox_vec(x, lambda = "auto", silent = FALSE)
#
# box_cox_inv_vec(x, lambda)
#
# auto_lambda(
#   x,
#   method = c("guerrero", "loglik"),
#   lambda_lower = -1,
#   lambda_upper = 2
# )



# 1.準備 ---------------------------------------------------------------------

library(tidyverse)
library(timetk)


# データ準備
d10_daily <- m4_daily %>% filter(id == "D10")


# データ確認
d10_daily %>% print()
d10_daily %>% glimpse()



# 2.ベクトル操作 -------------------------------------------------------------

# Box-Cox変換
value_bc <- d10_daily$value %>% box_cox_vec()
value_bc %>% print()



# 3.データフレーム操作 ---------------------------------------------------------

# Box-Cox変換
df_box_cox <-
  m4_daily %>%
    group_by(id) %>%
    mutate(value_bc = box_cox_vec(value))


# データ確認
df_box_cox %>% print()
df_box_cox %>% summary()



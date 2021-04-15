# Title     : tk_anomaly_diagnostics
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/23
# URL       : https://business-science.github.io/timetk/reference/tk_anomaly_diagnostics.html



# ＜ポイント＞





# ＜構文＞
# tk_anomaly_diagnostics(
#   .data,
#   .date_var,
#   .value,
#   .frequency = "auto",
#   .trend = "auto",
#   .alpha = 0.05,
#   .max_anomalies = 0.2,
#   .message = TRUE
# )
#
## S3 method for data.frame
# tk_anomaly_diagnostics(
#   .data,
#   .date_var,
#   .value,
#   .frequency = "auto",
#   .trend = "auto",
#   .alpha = 0.05,
#   .max_anomalies = 0.2,
#   .message = TRUE
# )



# 1.準備 --------------------------------------------------------

library(tidyverse)
library(timetk)



# データ確認
walmart_sales_weekly


# 使用データ
walmart_sales_weekly %>% select(id, Date, Weekly_Sales)


# レコード件数
walmart_sales_weekly %>%
  filter(id %in% c("1_1", "1_3")) %>%
  group_by(id) %>%
  tally()



# 2.異常検知 --------------------------------------------------------

# 異常検知
# --- データ出力
walmart_sales_weekly %>%
    filter(id %in% c("1_1", "1_3")) %>%
    group_by(id) %>%
    tk_anomaly_diagnostics(Date, Weekly_Sales)
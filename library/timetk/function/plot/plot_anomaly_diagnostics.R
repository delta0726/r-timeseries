# Title     : plot_anomaly_diagnostics
# Objective : TODO
# Created by: Owner
# Created on: 2020/8/23
# URL       : https://business-science.github.io/timetk/reference/plot_anomaly_diagnostics.html



# ＜ポイント＞





# ＜構文＞
# plot_anomaly_diagnostics(
#  .data,
#  .date_var,
#  .value,
#  .facet_vars = NULL,
#  .frequency = "auto",
#  .trend = "auto",
#  .alpha = 0.05,
#  .max_anomalies = 0.2,
#  .message = TRUE,
#  .facet_ncol = 1,
#  .facet_scales = "free",
#  .line_color = "#2c3e50",
#  .line_size = 0.5,
#  .line_type = 1,
#  .line_alpha = 1,
#  .anom_color = "#e31a1c",
#  .anom_alpha = 1,
#  .anom_size = 1.5,
#  .ribbon_fill = "grey20",
#  .ribbon_alpha = 0.2,
#  .legend_show = TRUE,
#  .title = "Anomaly Diagnostics",
#  .x_lab = "",
#  .y_lab = "",
#  .color_lab = "Anomaly",
#  .interactive = TRUE
# )





# 1.準備 --------------------------------------------------------

library(dplyr)
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
# --- プロット出力
walmart_sales_weekly %>%
    filter(id %in% c("1_1", "1_3")) %>%
    group_by(id) %>%
    plot_anomaly_diagnostics(Date, Weekly_Sales,
                             .message = FALSE,
                             .facet_ncol = 3,
                             .ribbon_alpha = 0.25,
                             .interactive = FALSE)
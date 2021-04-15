# Title     : 1 specify
# Objective : TODO
# Created by: Owner
# Created on: 2020/7/12
# URL       : https://infer.netlify.app/reference/specify.html
#           : https://infer.netlify.app/articles/infer.html


# ＜ポイント＞




# 1.準備 ----------------------------------------


library(tidyverse)
library(timetk)

# Setup for the plotly charts (# FALSE returns ggplots)
interactive <- FALSE



# 2.Grouped ACF Diagnostics ----------------------------------------

# データ確認
m4_hourly %>% print()
m4_hourly %>% group_by(id) %>% tally()


# ACF診断プロット
# --- ACF & PACFの両方を表示
m4_hourly %>%
  group_by(id) %>%
  plot_acf_diagnostics(
      .date_var = date,
      .value = value,
      .lags = "7 days",          # 7-Days of hourly lags
      .interactive = interactive
  )


# 3.Grouped CCF Plots -----------------------------------------------

# データ確認
walmart_sales_weekly %>% print()
walmart_sales_weekly %>% group_by(id) %>% tally()


# CCF診断プロット
walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales, Temperature, Fuel_Price) %>%
  group_by(id) %>%
  plot_acf_diagnostics(
      .date_var = Date,
      .value = Weekly_Sales,
      .ccf_vars = c(Temperature, Fuel_Price),
      .lags = "3 months",
      .interactive = interactive
  )


# 4.Grouped Seasonal Visualizations --------------------------------


# データ確認
taylor_30_min %>% print()
m4_hourly %>% group_by(id) %>% tally() %>%


taylor_30_min %>%
    plot_seasonal_diagnostics(date, value, .interactive = interactive)


m4_hourly %>%
    group_by(id) %>%
    plot_seasonal_diagnostics(date, value, .interactive = interactive)



# 5.STL Diagnostics -------------------------------------------------

m4_hourly %>%
    group_by(id) %>%
    plot_stl_diagnostics(
        date, value,
        .frequency = "auto", .trend = "auto",
        .feature_set = c("observed", "season", "trend", "remainder"),
        .interactive = interactive)
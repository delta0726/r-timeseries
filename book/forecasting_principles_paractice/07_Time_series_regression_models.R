# ******************************************************************************
# Chapter   : 7 Time series regression models
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/24
# URL       : https://otexts.com/fpp3/regression.html
# ******************************************************************************


# ＜概要＞
# - 関心のある時系列(Y)が他の時系列(X)と線形関係にあると仮定して予測する
#   --- たとえば、総広告費(X)を予測子として使用して月間売上(Y)を予測したい



# 0 準備 ---------------------------------------------------------------------------------------

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(GGally)
library(fpp3)


# データ確認
us_change %>% print()
us_change %>% glimpse()
us_change$Quarter %>% table()


# プロット作成
# --- 時系列チャート
# --- ｢Consumption｣と｢Income｣には連動性がありそうだ
us_change %>%
  pivot_longer(c(Consumption, Income), names_to = "Series") %>%
  autoplot(value) +
  labs(y = "% change")

# プロット作成
# --- 散布図
us_change %>%
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly % change)",
       x = "Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# モデル適用
# --- TSLM: Time-Series Linear Model
us_change %>%
  model(TSLM(Consumption ~ Income)) %>%
  report()

# プロット作成
# --- 散布図行列
us_change %>% ggpairs(columns = 2:6)



# 7.2 Least squares estimation ---------------------------------------


fit.consMR <-
  us_change %>%
    model(tslm = TSLM(Consumption ~ Income + Production + Unemployment + Savings))

# 確認
fit.consMR %>% report()

# プロット作成
fit.consMR %>%
  augment() %>%
    ggplot(aes(x = Quarter)) +
    geom_line(aes(y = Consumption, colour = "Data")) +
    geom_line(aes(y = .fitted, colour = "Fitted")) +
    labs(y = NULL,
         title = "Percent change in US consumption expenditure") +
    scale_color_manual(values=c(Data="black",Fitted="red")) +
    guides(colour = guide_legend(title = NULL))

# プロット作成
fit.consMR %>%
  augment() %>%
    ggplot(aes(x = Consumption, y = .fitted)) +
    geom_point() +
    labs(y = "Fitted (predicted values)",
         x = "Data (actual values)",
         title = "Percent change in US consumption expenditure") +
    geom_abline(intercept = 0, slope = 1)



# 7.3 Evaluating the regression model -----------------------------


fit.consMR %>% gg_tsresiduals()

fit.consMR %>%
  augment() %>%
  features(.innov, ljung_box, lag = 10, dof = 5)



df <-
  us_change %>%
    left_join(residuals(fit.consMR), by = "Quarter")

p1 <-
  df %>%
    ggplot(aes(x = Income, y = .resid)) +
    geom_point() +
    labs(y = "Residuals")


p2 <-
  df %>%
    ggplot(aes(x = Production, y = .resid)) +
    geom_point() +
    labs(y = "Residuals")

p3 <-
  df %>%
    ggplot(aes(x = Savings, y = .resid)) +
    geom_point() +
    labs(y = "Residuals")

p4 <-
  df %>%
    ggplot(aes(x = Unemployment, y = .resid)) +
    geom_point() +
    labs(y = "Residuals")

(p1 | p2) / (p3 | p4)


fit.consMR %>%
  augment() %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() + labs(x = "Fitted", y = "Residuals")


fit <-
  aus_airpassengers %>%
    filter(Year <= 2011) %>%
    left_join(guinea_rice, by = "Year") %>%
    model(TSLM(Passengers ~ Production))

fit %>% report()

fit %>% gg_tsresiduals()



# 7.4 Some useful predictors -------------------------------------


recent_production <-
  aus_production %>%
    filter(year(Quarter) >= 1992)

recent_production %>%
  autoplot(Beer) +
  labs(y = "Megalitres")

recent_production <-
  aus_production %>%
    filter(year(Quarter) >= 1992)

recent_production %>%
  autoplot(Beer) +
  labs(y = "Megalitres")


fit_beer <-
  recent_production %>%
    model(TSLM(Beer ~ trend() + season()))

fit_beer %>% report()


fit_beer %>%
  augment() %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_color_manual(values = c(Data = "black", Fitted = "red")) +
  labs(y = "Megalitres",
       title = "Quarterly Beer Production") +
  guides(colour = guide_legend(title = "Series"))


fit_beer %>%
  augment() %>%
  ggplot(aes(x = Beer, y = .fitted,
             colour = factor(quarter(Quarter)))) +
  geom_point() +
  labs(y = "Fitted", x = "Actual values",
       title = "Quarterly beer production") +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))


fourier_beer <-
  recent_production %>%
    model(TSLM(Beer ~ trend() + fourier(K = 2)))

fourier_beer %>% report()


# 7.5 Selecting predictors -----------------------------


fit.consMR %>%
  glance() %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)


# 7.6 Forecasting with regression------------------------

recent_production <-
  aus_production %>%
    filter(year(Quarter) >= 1992)

fit_beer <-
  recent_production %>%
    model(TSLM(Beer ~ trend() + season()))

fc_beer <- fit_beer %>% forecast()

fc_beer %>%
  autoplot(recent_production) +
  labs(title = "Forecasts of beer production using regression",
       y = "megalitres")



fit_consBest <-
  us_change %>%
    model(lm = TSLM(Consumption ~ Income + Savings + Unemployment))

future_scenarios <-
  scenarios(Increase =
              new_data(us_change, 4) %>%
                mutate(Income=1, Savings=0.5, Unemployment=0),
            Decrease =
              new_data(us_change, 4) %>%
                mutate(Income=-1, Savings=-0.5, Unemployment=0),
            names_to = "Scenario")

fc <-
  fit_consBest %>%
    forecast(new_data = future_scenarios)


us_change %>%
  autoplot(Consumption) +
  autolayer(fc) +
  labs(y = "% change in US consumption")



fit_cons <-
  us_change %>%
    model(TSLM(Consumption ~ Income))

new_cons <- scenarios(
  "Average increase" = new_data(us_change, 4) %>% mutate(Income = mean(us_change$Income)),
  "Extreme increase" = new_data(us_change, 4) %>% mutate(Income = 12),
  names_to = "Scenario"
)
fcast <- forecast(fit_cons, new_cons)

us_change %>%
  autoplot(Consumption) +
  autolayer(fcast) +
  labs(y = "% change in US consumption")



# 7.7 Nonlinear regression -------------------------------------


boston_men <- boston_marathon %>%
  filter(Year >= 1924) %>%
  filter(Event == "Men's open division") %>%
  mutate(Minutes = as.numeric(Time)/60)


fit_trends <- boston_men %>%
  model(
    linear = TSLM(Minutes ~ trend()),
    exponential = TSLM(log(Minutes) ~ trend()),
    piecewise = TSLM(Minutes ~ trend(knots = c(1950, 1980)))
  )
fc_trends <- fit_trends %>% forecast(h = 10)

boston_men %>%
  autoplot(Minutes) +
  geom_line(aes(y = .fitted, colour = .model), data = fitted(fit_trends)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(y = "Winning times in minutes",
       title = "Boston Marathon")


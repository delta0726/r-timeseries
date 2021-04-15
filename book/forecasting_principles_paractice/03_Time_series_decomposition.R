# ******************************************************************************
# Chapter   : 3 Time series decomposition
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/24
# URL       : https://otexts.com/fpp3/decomposition.html
# ******************************************************************************

# ＜概要＞





# 0 準備 ---------------------------------------------------------------------------------------

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)

# 3.1 Transformations and adjustments ---------------------

global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(GDP/Population) + labs(y = "GDP per capita ($US)")


print_retail <- aus_retail %>%
  filter(Industry == "Newspaper and book retailing") %>%
  group_by(Industry) %>%
  index_by(Year = year(Month)) %>%
  summarise(Turnover = sum(Turnover))
aus_economy <- global_economy %>%
  filter(Code == "AUS")
print_retail %>%
  left_join(aus_economy, by = "Year") %>%
  mutate(Adjusted_turnover = Turnover / CPI * 100) %>%
  pivot_longer(c(Turnover, Adjusted_turnover),
               values_to = "Turnover") %>%
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(
    y = "$A",
    title = "Turnover for the Australian print media industry"
  )



lambda <- aus_production %>%
  features(Gas, features = guerrero) %>%
  pull(lambda_guerrero)
aus_production %>% autoplot(box_cox(Gas, lambda))


# 3.2 Time series components --------------------------


us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)
autoplot(us_retail_employment, Employed) +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

dcmp <- us_retail_employment %>%
  model(STL(Employed))
components(dcmp)


autoplot(us_retail_employment, Employed, color = "gray") +
  autolayer(components(dcmp), trend, color = "red") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

components(dcmp) %>% autoplot()


autoplot(us_retail_employment, Employed, color = "gray") +
  autolayer(components(dcmp), season_adjust, color = "blue") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )


# 3.3 Moving averages --------------------------------------


global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Total Australian exports")


aus_exports <- global_economy %>%
  filter(Country == "Australia") %>%
  mutate(
    `5-MA` = slider::slide_dbl(Exports, mean,
                .before = 2, .after = 2, .complete = TRUE)
  )



autoplot(aus_exports, Exports) +
  autolayer(aus_exports, `5-MA`, color = "red") +
  labs(
    y = "% of GDP",
    title = "Total Australian exports"
  ) +
  guides(colour = guide_legend(title = "series"))


beer <- aus_production %>%
  filter(year(Quarter) >= 1992) %>%
  select(Quarter, Beer)
beer_ma <- beer %>%
  mutate(
    `4-MA` = slider::slide_dbl(Beer, mean,
                .before = 1, .after = 2, .complete = TRUE),
    `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
                .before = 1, .after = 0, .complete = TRUE)
  )

us_retail_employment_ma <- us_retail_employment %>%
  mutate(
    `12-MA` = slider::slide_dbl(Employed, mean,
                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                .before = 1, .after = 0, .complete = TRUE)
  )
autoplot(us_retail_employment_ma, Employed, color = "gray") +
  autolayer(us_retail_employment_ma, vars(`2x12-MA`),
            color = "red") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")


# 3.4 Classical decomposition --------------------

us_retail_employment %>%
  model(
    classical_decomposition(Employed, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition of total
                  US retail employment")


# 3.5 X11 decomposition -----------------------------

x11_dcmp <- us_retail_employment %>%
  model(x11 = feasts:::X11(Employed, type = "additive")) %>%
  components()
autoplot(x11_dcmp) +
  labs(title =
    "Additive X11 decomposition of US retail employment")


x11_dcmp %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Employed, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail") +
  scale_colour_manual(
    values = c("gray", "blue", "red"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

x11_dcmp %>%
  gg_subseries(seasonal)


# 3.6 SEATS decomposition ----------------------

seats_dcmp <- us_retail_employment %>%
  model(seats = feasts:::SEATS(Employed)) %>%
  components()
autoplot(seats_dcmp) +
  labs(title =
    "SEATS decomposition of total US retail employment")



# 3.7 STL decomposition --------------------------

us_retail_employment %>%
  model(
    STL(Employed ~ trend(window = 7) +
              season(window = "periodic"),
    robust = TRUE)) %>%
  components() %>%
  autoplot()

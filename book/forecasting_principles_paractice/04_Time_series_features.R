# ******************************************************************************
# Chapter   : 4 Time series features
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/24
# URL       : https://otexts.com/fpp3/features.html
# ******************************************************************************

# ＜概要＞





# 0 準備 ---------------------------------------------------------------------------------------

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)

# 4.1 Some simple statistics

tourism %>%
  features(Trips, list(mean = mean)) %>%
  arrange(mean)

tourism %>% features(Trips, quantile)


# 4.2 ACF features -----------------------------------------------

tourism %>% features(Trips, feat_acf)


# 4.3 STL Features ---------------------------------------------


tourism %>%
  features(Trips, feat_stl)


# 4.5 Exploring Australian tourism data --------------------------

tourism_features <- tourism %>%
  features(Trips, feature_set(pkgs = "feasts"))
tourism_features

library(glue)
tourism_features %>%
  select_at(vars(contains("season"), Purpose)) %>%
  mutate(
    seasonal_peak_year = glue("Q{seasonal_peak_year+1}"),
    seasonal_trough_year = glue("Q{seasonal_trough_year+1}"),
  ) %>%
  GGally::ggpairs(mapping = aes(colour = Purpose))


library(broom)
pcs <- tourism_features %>%
  select(-State, -Region, -Purpose) %>%
  prcomp(scale = TRUE) %>%
  augment(tourism_features)
pcs %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() +
  theme(aspect.ratio = 1)

outliers <- pcs %>%
  filter(.fittedPC1 > 10) %>%
  select(Region, State, Purpose, .fittedPC1, .fittedPC2)
outliers


outliers %>%
  left_join(tourism, by = c("State", "Region", "Purpose")) %>%
  mutate(
    Series = glue("{State}", "{Region}", "{Purpose}",
                  .sep = "\n\n")
  ) %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  labs(title = "Outlying time series in PC space")




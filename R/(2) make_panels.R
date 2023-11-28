# use annual panel for five- and ten-year datasets for OLS 
# only for use in appendix 
# also creates subsetted data and missing covariate imputation with mice package 

library(tidyverse)
library(texreg)
library(readxl)
library(mice)
rm(list = ls()) 

# load data ---------------------------------------------------------------

boix_rr <- read.csv("Data/Data Output/boix_rr.csv")

# make five-year data -----------------------------------------------------

boix_five <- boix_rr %>% 
  mutate(five_year_period = 1800 + ((year - 1800) %/% 5) * 5) %>%
  group_by(five_year_period, cown, cc3) %>% 
  summarise(average_crisis_tally = mean(crisis_tally, na.rm = TRUE), 
            average_banking_crisis = mean(banking_crisis, na.rm = T), 
            average_currency_crisis = mean(currency_crises, na.rm = T), 
            average_domestic_debt_crisis = mean(domestic_debt_in_default, na.rm = T), 
            average_external_debt_crisis = mean(external_default_in_1975, na.rm=T), 
            average_inflation_crisis = mean(inflation_crises, na.rm= T), 
            average_gdppc = mean(gdppc, na.rm = T), 
            average_pop = mean(pop, na.rm = T), 
            average_growth = mean(percent_growth, na.rm=T), 
            democracy = ifelse(any(democracy == 1 & lag(democracy) == 0) | all(democracy == 1), 1, 0), 
            independence = mean(independence, na.rm = T), 
            gold_standard = mean(gold_standard, na.rm = T))

unique(boix_five$five_year_period)
write.csv(boix_five, "Data/Data Output/boix_five.csv")

# make ten-year data ------------------------------------------------------

boix_ten <- boix_rr %>%
  mutate(ten_year_period = 1800 + ((year - 1800) %/% 10) * 10) %>%
  group_by(ten_year_period, cown, cc3) %>%
  summarise(average_crisis_tally = mean(crisis_tally, na.rm = TRUE),
            average_banking_crisis = mean(banking_crisis, na.rm = TRUE), 
            average_currency_crisis = mean(currency_crises, na.rm = T), 
            average_domestic_debt_crisis = mean(domestic_debt_in_default, na.rm = T), 
            average_external_debt_crisis = mean(external_default_in_1975, na.rm=T), 
            average_inflation_crisis = mean(inflation_crises, na.rm= T), 
            average_gdppc = mean(gdppc, na.rm = T), 
            average_pop = mean(pop, na.rm = T), 
            average_growth = mean(percent_growth, na.rm=T), 
            democracy = ifelse(any(democracy == 1 & lag(democracy) == 0) | all(democracy == 1), 1, 0), 
            independence = mean(independence, na.rm = T), 
            gold_standard = mean(gold_standard, na.rm = T))

write.csv(boix_ten, "Data/Data Output/boix_ten.csv")

# make subsetted data set ------------------------------------------------

boix_subset <- boix_rr %>% 
  filter(year %in% c(1870:2016))

write.csv(boix_subset, "Data/Data Output/boix_subset.csv")

# impute missing data to annual data frame ---------------------------------------------------------------

boix_covariates <- boix_rr %>% 
  select(year, cc3, gdppc, pop, percent_growth)

boix_imputed <- mice(boix_covariates, m = 5, 
                     method = "pmm", seed = 123)

completed_data <- complete(boix_imputed) %>% 
  rename(gdppc_impute = gdppc, 
         pop_impute = pop, 
         percent_growth_impute = percent_growth)

class(completed_data)
boix_rr <- read.csv("Data/Data Output/boix_rr.csv")

boix_rr <- boix_rr %>% 
  left_join(completed_data, by = c("year", "cc3"))

write.csv(boix_rr, "Data/Data Output/boix_rr.csv")

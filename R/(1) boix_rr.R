# script combines Boix data with Reinhart data from 1800-2016.  

library(tidyverse)
library(fixest)
library(haven)
library(janitor)
library(labelled)
library(readxl)
library(countrycode)
rm(list = ls())

# load data ---------------------------------------------------------------

# contains codes from iso3c (ISO (2/3-character and numeric))
# contains codes from correlates of war (cown)

boix_data <- read_csv("Data/Raw Data/dataverse_files (5)/democracy-v4.0.csv") %>% 
  filter(year <= 2016) %>% 
  mutate(country = str_to_title(country)) %>% 
  select(cown = ccode, iso3c = abbreviation, cowcodes = abbreviation_undp, year, democracy) 

boix_data <- boix_data %>% 
  mutate(iso3n =  countrycode(iso3c, origin = "iso3c", destination = "iso3n")) %>% 
  mutate(country_name = countrycode(cown, origin = "cown", destination = "country.name")) %>% 
  select(cown, cowcodes, iso3c, iso3n, country_name, year, democracy) 

rr_crisis_data <- read_excel("Data/Raw Data/financial_crisis_data.xlsx")

# clean title names from RR
rr_crisis_data <- labelled::foreign_to_labelled(rr_crisis_data) # makes the stata labels into R
rr_crisis_data <- sjlabelled::label_to_colnames(rr_crisis_data)
colnames(rr_crisis_data) <- tolower(colnames(rr_crisis_data))
colnames(rr_crisis_data) <- janitor::make_clean_names(colnames(rr_crisis_data))
rr_crisis_data <- rr_crisis_data[-1, ]
colnames(rr_crisis_data)

rr_crisis_data <- rr_crisis_data %>%  
  select(case, cc3, country, year, gold_standard,independence, banking_crisis, systemic_crisis, 
         domestic_debt_in_default, currency_crises, inflation_crises, 
         external_default_ex_1975 = sovereign_external_debt_1_default_and_restructurings_1800_2012_does_not_include_defaults_on_wwi_debt_to_united_states_and_united_kingdom_and_post_1975_defaults_on_official_external_creditors, 
         external_default_in_1975 = sovereign_external_debt_2_default_and_restructurings_1800_2012_does_not_include_defaults_on_wwi_debt_to_united_states_and_united_kingdom_but_includes_post_1975_defaults_on_official_external_creditors)

rr_crisis_data <- rr_crisis_data %>% mutate_all(~ifelse(. == "n/a", NA, .))
rr_crisis_data <- rr_crisis_data %>% # make all numeric 
  mutate(independence = as.numeric(independence), 
         gold_standard = as.numeric(gold_standard), 
         currency_crises = as.numeric(currency_crises), 
         inflation_crises = as.numeric(inflation_crises), 
         external_default_ex_1975 = as.numeric(external_default_ex_1975), 
         external_default_in_1975 = as.numeric(external_default_in_1975), 
         domestic_debt_in_default = as.numeric(domestic_debt_in_default), 
         banking_crisis = as.numeric(banking_crisis), 
         systemic_crisis = as.numeric(systemic_crisis))
         
### create crisis tally variable 
## also create cown variable  
rr_crisis_data <- rr_crisis_data %>% 
  mutate(crisis_tally = banking_crisis + systemic_crisis + domestic_debt_in_default + currency_crises + 
           inflation_crises + external_default_in_1975) %>% 
  mutate(cown = countrycode(cc3, origin = 'iso3c', destination = 'cown')) %>% 
  mutate(country_name = countrycode(cown, origin = "cown", destination = "country.name")) 

write.csv(rr_crisis_data, "Data/Data Output/rr_crisis_data.csv")

# merge data --------------------------------------------------------------

rr_crisis_data <- rr_crisis_data %>% 
  select(!c(case, country, country_name))

boix_rr <- rr_crisis_data %>%  # merged on abbrev and year 
  full_join(boix_data, by = c("cown", "year"))

# add control data  -------------------------------------------------------

control_data <- read_excel("Data/Raw Data/mpd2020 (2).xlsx", 3) %>% 
  filter(year %in% c(1800:2016), countrycode %in% boix_rr$cc3) 

boix_rr <- boix_rr %>% 
  left_join(control_data, by = c("year", "cc3" = "countrycode")) %>% 
  mutate(percent_growth = (gdppc - lag(gdppc)) / lag(gdppc) * 100) %>% 
  mutate(log_gdppc = log(gdppc + 1)) %>% 
  filter(!is.na(cc3)) # some NAs because there is more data 

unique(boix_rr$cc3)

write.csv(boix_rr, "Data/Data Output/boix_rr.csv")

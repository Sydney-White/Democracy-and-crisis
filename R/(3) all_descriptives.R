# all descriptives 

library(tidyverse)
library(stargazer)
library(panelView)
library(gridExtra)
library(sf)
rm(list = ls()) 

####### Appendix Time Series 
rr_crisis_data <- read.csv("Data/Data Output/rr_crisis_data.csv")  

crisis_year <- rr_crisis_data %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(crisis_year = sum(crisis_tally, na.rm = T))

p1 <- ggplot(crisis_year, aes(x = year, y = crisis_year)) + 
  geom_bar(stat = "identity", color = "dodgerblue") + 
  theme_bw() + 
  xlab("") +
  ylab("Count of Ongoing Crises") +
  theme(axis.title.y = element_text(size = 24), 
        axis.text.y = element_text(size = 24), 
        axis.text.x = element_text(size = 24))
ggsave(plot = p1, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/crisis_count.png",
       device = png, bg = "white", width = 12.28, height = 9.01, units = "in")

#######################################
# descriptive for the relative rates 
#######################################

boix_data <- read.csv("Data/Data Output/boix_rr.csv") 

boix_data_democracy <- boix_data %>% filter(democracy == 1)
boix_data_autocracy <- boix_data %>% filter(democracy == 0)

sum(!is.na(boix_data_democracy$external_default_ex_1975))
sum(!is.na(boix_data_autocracy$external_default_ex_1975))

###### for text 

mean(boix_data_democracy$crisis_tally, na.rm = T)
mean(boix_data_autocracy$crisis_tally, na.rm = T)

mean(boix_data_democracy$external_default_ex_1975, na.rm = T)
mean(boix_data_autocracy$external_default_ex_1975, na.rm = T)

mean(boix_data_democracy$banking_crisis, na.rm = T)
mean(boix_data_autocracy$banking_crisis, na.rm = T)

###### for Figure 2 

avg_regime <- data.frame(
  Category = c("crisis_tally", "external_default", "banking_crisis"),
  Democracy = c(mean(boix_data_democracy$crisis_tally, na.rm = T),
                mean(boix_data_democracy$external_default_in_1975, na.rm = T),
                mean(boix_data_democracy$banking_crisis, na.rm = T)),
  Autocracy = c(mean(boix_data_autocracy$crisis_tally, na.rm = T),
                mean(boix_data_autocracy$external_default_in_1975, na.rm = T), 
                mean(boix_data_autocracy$banking_crisis, na.rm = T))
)


avg_regime$Category <- factor(avg_regime$Category, levels = c("banking_crisis", "external_default", "crisis_tally"))

avg_regime_long <- avg_regime %>%
  gather(Variable, Value, -Category)

custom_labels <- c(
  "external_default" = "External Debt",
  "crisis_tally" = "Crisis Tally",
  "banking_crisis" = "Banking Crisis"
)
p <- ggplot(avg_regime_long, aes(x = Category, y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(x = "", y = "Mean Value of Sample") +
  scale_fill_manual(values = c(Democracy = "navyblue", 
                               Autocracy = "wheat")) +
  theme_bw() + 
  theme(
    legend.position = "bottom", 
    legend.box = "horizontal",  
    legend.title = element_blank() 
  ) + 
  scale_x_discrete(labels = custom_labels) +
  coord_flip() 

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/avg_value.png",
       device = png, bg = "white", width = 4.88, height = 3.34, units = "in")

### Figure 2 
 
boix_five <- read.csv("Data/Data Output/boix_five.csv")
boix_ten <- read.csv("Data/Data Output/boix_ten.csv")

boix_data$crisis_tally <- as.numeric(as.character(boix_data$crisis_tally))
boix_data$democracy <- as.numeric(as.character(boix_data$democracy))
boix_five$average_crisis_tally <- as.numeric(as.character(boix_five$average_crisis_tally))
boix_five$democracy <- as.numeric(as.character(boix_five$democracy))
boix_ten$average_crisis_tally <- as.numeric(as.character(boix_ten$average_crisis_tally))
boix_ten$democracy <- as.numeric(as.character(boix_ten$democracy))

p <- panelview(crisis_tally ~ democracy,
          data = boix_data,
          index = c("cc3", "year"),
          main = "",
          background = "white",
          xlab = "", ylab = "",
          axis.adjust = TRUE, axis.lab.gap = c(9,2))
ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/panel_data_dict.png",
       device = png, bg = "white", width = 4.88, height = 3.34, units = "in")

p <- panelview(average_crisis_tally ~ democracy,
               data = boix_five,
               index = c("cc3", "five_year_period"),
               main = "",
               background = "white",
               xlab = "", ylab = "",
               cex.axis.x = 7,
               axis.adjust = TRUE, axis.lab.gap = c(0,2))
ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/treatment_status_five.png",
       device = png, bg = "white", width = 4.88, height = 3.34, units = "in")

p <- panelview(average_crisis_tally ~ democracy,
               data = boix_ten,
               index = c("cc3", "ten_year_period"),
               main = "",
               background = "white",
               xlab = "", ylab = "",
               axis.adjust = TRUE, axis.lab.gap = c(0,2))

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/treatment_status_ten.png",
       device = png, bg = "white", width = 4.88, height = 3.34, units = "in")

# general table -----------------------------------------------------------

boix_data_no_id <- boix_data %>% select(!c("X.1", "X", "year", "cown", "iso3n", 
                                           "external_default_ex_1975", "gold_standard", 
                                           "independence")) 

stargazer(boix_data_no_id, 
          header = F, min.max = F, iqr = T,
          label="descriptive_table", align = T,
          title="Summary Statistics",
          out = "~/Dropbox/Apps/Overleaf/Writing Sample/appendix/descriptives.tex", 
          font.size = "small", 
          covariate.labels = c("Banking Crisis", 
                            "Systemic Crisis", "Domestic Debt Default", 
                            "Currency Crisis", "Inflation Crisis", "External Default", 
                            "Crisis Tally", "Democracy", "GDP Per Capita", 
                            "Population (in thousands)", "Percent Growth", 
                            "Log GDP Per Capita", "Imputed Log GDP Per Capita", 
                            "Imputed Population", "Imputed Percent Growth"))

# table for observations for panel ----------------------------------------
# 
# boix_rr <- read.csv("Data/boix_rr.csv") 
# unique(boix_rr$region)
# entry_data <- boix_rr %>%
#   group_by(country, region) %>%
#   summarise(entry = min(year), no_observations = n()) %>% 
#   filter(!is.na(country))
# 
# entry_data_americas <- entry_data %>% filter(region == "Americas")
# entry_data_africa <- entry_data %>% filter(region == "Africa") 
# entry_data_europe <- entry_data %>% filter(region == "Europe") 
# entry_data_oceania <- entry_data %>% filter(region == "Oceania") 
# entry_data_asia <- entry_data %>% filter(region == "Asia")
# 
# stargazer(entry_data_europe, header = F, 
#           label = "obs_entry_europe", 
#           rownames = FALSE,
#           summary = FALSE, 
#           title = "Country Entry Year and Number of Observations, Europe", 
#           out = "~/Dropbox/Apps/Overleaf/Writing Sample/entry_year_europe.tex")
# stargazer(entry_data_americas, header = F, 
#           label = "obs_entry_americas", 
#           rownames = FALSE,
#           summary = FALSE, 
#           title = "Country Entry Year and Number of Observations, Americas", 
#           out = "~/Dropbox/Apps/Overleaf/Writing Sample/entry_year_americas.tex")
# stargazer(entry_data_africa, header = F, 
#           label = "obs_entry_africa", 
#           rownames = FALSE,
#           summary = FALSE, 
#           title = "Country Entry Year and Number of Observations, Africa", 
#           out = "~/Dropbox/Apps/Overleaf/Writing Sample/entry_year_africa.tex")
# stargazer(entry_data_oceania, header = F, 
#           label = "obs_entry_oceania", 
#           rownames = FALSE,
#           summary = FALSE, 
#           title = "Country Entry Year and Number of Observations, Oceania", 
#           out = "~/Dropbox/Apps/Overleaf/Writing Sample/entry_year_oceania.tex")
# stargazer(entry_data_asia, header = F, 
#           label = "obs_entry_asia", 
#           rownames = FALSE,
#           summary = FALSE, 
#           title = "Country Entry Year and Number of Observations, Asia", 
#           out = "~/Dropbox/Apps/Overleaf/Writing Sample/entry_year_asia.tex")

#################################
# Appendix Figure 
#################################

boix_sum <- boix_data %>% 
  group_by(cc3) %>% 
  summarise(num_debt_crises = sum(external_default_ex_1975, na.rm=T), 
            num_banking_crises = sum(banking_crisis, na.rm = T)) %>% 
  arrange(num_debt_crises) 

boix_sum$cc3 <- factor(boix_sum$cc3, levels = boix_sum$cc3)

p <- ggplot(boix_sum, aes(x = cc3, y = num_debt_crises)) +
  geom_bar(stat = "identity", fill = "navyblue") +
  labs(x = "",
       y = "Number of External Debt Crises") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/appendix/debt_crisis_dist.png",
       device = png, bg = "white", width = 4.88, height = 3.34, units = "in")

boix_sum <- boix_sum %>% 
  arrange(num_banking_crises) 

boix_sum$cc3 <- factor(boix_sum$cc3, levels = boix_sum$cc3)

p <- ggplot(boix_sum, aes(x = cc3, y = num_banking_crises)) +
  geom_bar(stat = "identity", fill = "wheat") +
  labs(x = "",
       y = "Number of Banking Crises") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))

mean(boix_sum$num_debt_crises)
mean(boix_sum$num_banking_crises)

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/appendix/banking_crisis_dist.png",
       device = png, bg = "white", width = 4.88, height = 3.34, units = "in")

######################################
# plot sample 
######################################

fe_1 <- feols(crisis_tally ~ democracy | cown + year,
              cluster=c("cown"), 
              data=boix_data)

analysis_data <- boix_data[obs(fe_1),]

nominal_sample <- read_sf("Data/dataverse_files (2) 3/world_countries_boundary_file_world_2002.shp") %>% 
  dplyr::mutate(sample = ifelse(ISO_3_CODE %in% unique(analysis_data$cc3), "Included", "Excluded")) %>% 
  mutate(sample = as.factor(sample))

p <-ggplot(nominal_sample, aes(fill = sample)) +
  geom_sf() +
  theme_bw() +
  scale_fill_manual(values=c("#69b3a2", "#404080")) + 
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/nominal.pdf",
       device = pdf, bg = "white", width = 7.84, height = 5.46, units = "in")

  
# Shows panel match example for Portugal's 1911 Democratization 

library(tidyverse)
library(PanelMatch)
library(sf)
rm(list = ls()) 

###### appendix for portugal 

boix_rr <- read.csv("Data/boix_rr.csv") 

match_1 <- PanelMatch(lag = 5, # what it is matching on
                      lead = 0:10, # number of periods in future 
                      time.id = 'year',
                      unit.id = "cown",
                      treatment = 'democracy',
                      outcome.var = 'banking_crisis',
                      refinement.method = 'ps.weight',
                      covs.formula = ~ I(lag(gdppc, 1:4)) + I(lag(pop, 1:4)) + I(lag(percent_growth, 1:4)), 
                      qoi = 'att',
                      data = boix_rr)

match_1$att # grouping 

##################### show portugal for visualization purposes 

matched_countries <- match_1[["att"]][["235.1911"]]
df_annual_matched <- boix_rr %>% 
  filter(cown %in% matched_countries) %>% 
  distinct(cown, cc3)
df_annual_matched$cc3

world_shapefiles <- read_sf("Data/dataverse_files (2) 3/world_countries_boundary_file_world_2002.shp") %>% 
  dplyr::mutate(match = ifelse(ISO_3_CODE %in% df_annual_matched$cc3, "Matched", "Not matched")) %>% 
  mutate(match = as.factor(match))

p <- ggplot(world_shapefiles, aes(fill = match)) +
  geom_sf() +
  theme_bw() +
  scale_fill_manual(values = brewer.pal(3, "Set1")) + 
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position = "bottom")

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/appendix/portugal_example.pdf",
       device = pdf, bg = "white", width = 10, height = 7, units = "in")

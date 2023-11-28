# panel match appendix for missing data 

library(tidyverse)
library(PanelMatch)
rm(list = ls()) 

# load data ------------------------------------------------------------

boix_rr <- read.csv("Data/Data Output/boix_rr.csv")

############ following function plots estimates 

plot_matches <- function(mod_1, mod_2, title_string) {
  coef1 <- unname(mod_1[["estimates"]])
  se1 <- unname(mod_1[["standard.error"]])
  year <- c(0:10)
  coef_base <- cbind(year, case = "Base", coef1, se1)
  
  coef1 <- unname(mod_2[["estimates"]])
  se1 <- unname(mod_2[["standard.error"]])
  coef_control <- cbind(year, case = "Control", coef1, se1)
  
  df <- data.frame(rbind(coef_base, coef_control)) %>%
    mutate(se1 = as.numeric(se1)) %>%
    mutate(coef1 = as.numeric(coef1)) %>%
    mutate(case = as.factor(case)) %>%
    mutate(year = as.numeric(year))
  
  p <- ggplot(df, aes(x=year, y=coef1, fill=case, color = case)) +
    geom_point(position=position_dodge(0.5)) +
    scale_color_manual(values = c("black", "dodgerblue")) +
    geom_errorbar(aes(ymin=coef1-se1, ymax=coef1+se1), width=.2,
                  position=position_dodge(0.5)) +
    theme_bw() +
    geom_hline(yintercept=0, linetype = "dashed", color = "darkgrey") + 
    ylab("Coefficient") +
    ggtitle(title_string) + 
    xlab("") +
    theme(legend.position = "bottom") +
    scale_fill_discrete(labels = c("Base", "Control"))
  return(p)
}

####################################################
# first model regresses democracy on crisis tally - SUBSET
####################################################

ct_match_base <- PanelMatch(lag = 5, 
                            lead = 0:10, # number of periods in future 
                            time.id = 'year',
                            unit.id = "cown",
                            treatment = 'democracy',
                            outcome.var = 'crisis_tally',
                            refinement.method = 'none',
                            qoi = 'att',
                            data = boix_rr)

ct_match_base$att # grouping 

mod_1 <- PanelEstimate(ct_match_base, number.iterations = 10000, boix_rr)

ct_control_match <- PanelMatch(lag = 5, 
                               lead = 0:10, # number of periods in future 
                               time.id = 'year',
                               unit.id = "cown",
                               covs.formula = ~ I(lag(gdppc_impute, 1:5)) + 
                                 I(lag(pop_impute, 1:5)) + I(lag(percent_growth_impute, 1:5)), 
                               treatment = 'democracy',
                               outcome.var = 'crisis_tally',
                               refinement.method = 'ps.weight',
                               qoi = 'att',
                               data = boix_rr)

ct_control_match$att # grouping 

mod_2 <- PanelEstimate(ct_control_match, number.iterations = 10000, boix_rr)

p1 <- plot_matches(mod_1, mod_2, "Democracy on Crisis Tally")
ggsave(plot = p1, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/appendix/crisis_tally_impute.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

############################
# banking panel, subsetted to 1870-2016
############################

banking_match_base <- PanelMatch(lag = 5,
                                 lead = 0:10,
                                 time.id = 'year',
                                 unit.id = 'cown',
                                 treatment = 'democracy',
                                 outcome.var = 'banking_crisis',
                                 refinement.method = 'none',
                                 qoi = 'att',
                                 data = boix_rr)
banking_match_base$att # grouping
mod_1 <- PanelEstimate(banking_match_base, number.iterations = 10000, boix_rr)

banking_match_control <- PanelMatch(lag = 5,
                                    lead = 0:10,
                                    time.id = 'year',
                                    unit.id = 'cown',
                                    treatment = 'democracy',
                                    covs.formula = ~ I(lag(gdppc, 1:5)) + I(lag(pop, 1:5)) + I(lag(percent_growth, 1:5)), 
                                    outcome.var = 'banking_crisis',
                                    refinement.method = 'ps.weight',
                                    qoi = 'att',
                                    data = boix_rr)
banking_match_control$att # grouping
mod_2 <- PanelEstimate(banking_match_control, number.iterations = 10000, boix_rr)

p1 <- plot_matches(mod_1, mod_2, "Democracy on Banking Crisis")
ggsave(plot = p1, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/appendix/banking_crisis_impute.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

####################################################
# third model regresses external debt crisis on democracy 
####################################################

debt_match_base <- PanelMatch(lag = 5,
                              lead = 0:10,
                              time.id = 'year',
                              unit.id = 'cown',
                              treatment = 'democracy',
                              outcome.var = 'external_default_in_1975',
                              refinement.method = 'none',
                              qoi = 'att',
                              data = boix_rr)
debt_match_base$att # grouping
mod_1 <- PanelEstimate(debt_match_base, number.iterations = 10000, boix_rr)

debt_match_controls <- PanelMatch(lag = 5,
                                  lead = 0:10,
                                  time.id = 'year',
                                  unit.id = 'cown',
                                  match.missing = T,
                                  covs.formula = ~ I(lag(gdppc, 1:5)) + I(lag(pop, 1:5)) + I(lag(percent_growth, 1:5)), 
                                  treatment = 'democracy',
                                  outcome.var = 'external_default_in_1975',
                                  refinement.method = 'ps.weight',
                                  qoi = 'att',
                                  data = boix_rr)
debt_match_controls$att # grouping
mod_2 <- PanelEstimate(debt_match_controls, number.iterations = 10000, boix_rr)

p1 <- plot_matches(mod_1, mod_2, "Democracy on External Debt")
ggsave(plot = p1, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/appendix/external_debt_impute.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

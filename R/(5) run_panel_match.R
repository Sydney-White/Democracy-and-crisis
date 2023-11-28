# use panel match package from Imai, Kim, and Wang (2021)
# estimates treatment effects for Crisis Tally, External Debt, and Banking Crisis

rm(list = ls()) 
library(tidyverse)
library(PanelMatch)

# load data ---------------------------------------------------------------

boix_annual <- read.csv("Data/Data Output/boix_rr.csv") # annual data

boix_annual_na_clean <- boix_annual %>% # assumes non-systematic missingness 
  filter(!is.na(gdppc)) %>% 
  filter(!is.na(pop)) %>% 
  filter(!is.na(percent_growth)) 

sd(boix_annual$crisis_tally, na.rm = T) # for interpretation

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


plot_pretrends <- function(placebo_base, placebo_control, title_string) { 
  
  coef1 <- c(unname(placebo_base$estimates))
  se1 <- c(mean(placebo_base$bootstrapped.estimates[,1]),
           mean(placebo_base$bootstrapped.estimates[,2]),
           mean(placebo_base$bootstrapped.estimates[,3]),
           mean(placebo_base$bootstrapped.estimates[,4]))
  year <- c(-4, -3, -2, -1)
  coef_base <- cbind(year, case = "Base", coef1, se1)
  
  coef2 <- c(unname(placebo_control$estimates))
  se2 <- c(mean(placebo_control$bootstrapped.estimates[,1]),
           mean(placebo_control$bootstrapped.estimates[,2]),
           mean(placebo_control$bootstrapped.estimates[,3]),
           mean(placebo_control$bootstrapped.estimates[,4]))
  coef_control <- cbind(year, case = "Control", coef2, se2)
  
  df <- data.frame(rbind(coef_base, coef_control)) %>%
    mutate(se1 = as.numeric(se1)) %>%
    mutate(coef1 = as.numeric(coef1)) %>%
    mutate(case = as.factor(case)) %>%
    mutate(year = as.numeric(year))
  
  ggplot(df, aes(x=year, y=coef1, fill=case, color = case)) +
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
  
}

####################################################
# first model regresses democracy on crisis tally 
####################################################

ct_match_base <- PanelMatch(lag = 3, 
                      lead = 0:10, # number of periods in future 
                      time.id = 'year',
                      unit.id = "cown",
                      treatment = 'democracy',
                      outcome.var = 'crisis_tally',
                      refinement.method = 'none',
                      qoi = 'att',
                      data = boix_annual)

ct_match_base$att # grouping 

mod_1 <- PanelEstimate(ct_match_base, number.iterations = 10000, boix_annual)

ct_control_match <- PanelMatch(lag = 3, 
                    lead = 0:10, # number of periods in future 
                    time.id = 'year',
                    unit.id = "cown",
                    covs.formula = ~ I(lag(gdppc, 1:5)) + I(lag(pop, 1:5)) + I(lag(percent_growth, 1:5)), 
                    treatment = 'democracy',
                    outcome.var = 'crisis_tally',
                    refinement.method = 'ps.weight',
                    qoi = 'att',
                    data = boix_annual_na_clean)

ct_control_match$att # grouping 

mod_2 <- PanelEstimate(ct_control_match, number.iterations = 10000, boix_annual_na_clean)
plot(mod_2)
p1 <- plot_matches(mod_1, mod_2, "Democracy on Crisis Tally")
ggsave(plot = p1, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/panel_match/crisis_tally.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

get_covariate_balance(ct_control_match$att, boix_annual_na_clean, 
                      covariates = c("pop", "percent_growth", "gdppc"), 
                      plot = TRUE, ylim = c(-2,2), ylab = "Standard Deviation", 
                      legend = F, verbose = F)

legend("bottomright", legend = c("Population", "% Growth", "GDP Per Cap."),
       col = c("black", "red", "green"), pch = 19, bty = "n")

####################################################
# second model regresses banking crisis on democracy 
####################################################

banking_match_base <- PanelMatch(lag = 3,
                    lead = 0:10,
                    time.id = 'year',
                    unit.id = 'cown',
                    treatment = 'democracy',
                    outcome.var = 'banking_crisis',
                    refinement.method = 'none',
                    qoi = 'att',
                    data = boix_annual)
banking_match_base$att # grouping
mod_1 <- PanelEstimate(banking_match_base, number.iterations = 10000, boix_annual)

banking_match_control <- PanelMatch(lag = 3,
                      lead = 0:10,
                      time.id = 'year',
                      unit.id = 'cown',
                      treatment = 'democracy',
                      covs.formula = ~ I(lag(gdppc, 1:5)) + I(lag(pop, 1:5)) + I(lag(percent_growth, 1:5)), 
                      outcome.var = 'banking_crisis',
                      refinement.method = 'ps.weight',
                      qoi = 'att',
                      data = boix_annual_na_clean)
banking_match_control$att # grouping
mod_2 <- PanelEstimate(banking_match_control, number.iterations = 10000, boix_annual_na_clean)

p1 <- plot_matches(mod_1, mod_2, "Democracy on Banking Crisis")
ggsave(plot = p1, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/panel_match/banking_crisis.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

get_covariate_balance(banking_match_control$att, boix_annual_na_clean, 
                      covariates = c("pop", "percent_growth", "gdppc"), 
                      plot = TRUE, ylim = c(-2,2), legend = F, ylab = "Standard Deviation")

legend("bottomright", legend = c("Population", "% Growth", "GDP Per Cap."),
       col = c("black", "red", "green"), pch = 19, bty = "n")

####################################################
# third model regresses external debt crisis on democracy 
####################################################

debt_match_base <- PanelMatch(lag = 3,
                    lead = 0:10,
                    time.id = 'year',
                    unit.id = 'cown',
                    treatment = 'democracy',
                    outcome.var = 'external_default_in_1975',
                    refinement.method = 'none',
                    qoi = 'att',
                    data = boix_annual)
debt_match_base$att # grouping
mod_1 <- PanelEstimate(debt_match_base, number.iterations = 10000, boix_annual)

debt_match_controls <- PanelMatch(lag = 3,
                      lead = 0:10,
                      time.id = 'year',
                      unit.id = 'cown',
                      match.missing = T,
                      covs.formula = ~ I(lag(gdppc, 1:5)) + I(lag(pop, 1:5)) + I(lag(percent_growth, 1:5)), 
                      treatment = 'democracy',
                      outcome.var = 'external_default_in_1975',
                      refinement.method = 'ps.weight',
                      qoi = 'att',
                      data = boix_annual_na_clean)
debt_match_controls$att # grouping
mod_2 <- PanelEstimate(debt_match_controls, number.iterations = 10000, boix_annual_na_clean)

p1 <- plot_matches(mod_1, mod_2, "Democracy on External Debt")
ggsave(plot = p1, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/panel_match/external_debt.pdf",
       device = pdf, bg = "white", width = 4.88, height = 3.34, units = "in")

get_covariate_balance(debt_match_controls$att, boix_annual_na_clean, 
                      covariates = c("pop", "percent_growth", "gdppc"), 
                      plot = TRUE, ylim = c(-2,2), ylab = "Standard Deviation", 
                      legend = F)

legend("bottomright", legend = c("Population", "% Growth", "GDP Per Cap."),
       col = c("black", "red", "green"), pch = 19, bty = "n")

# run placebos ------------------------------------------------------------

placebo_base <- placebo_test( # run placebo tests for pretrends
  ct_match_base,
  boix_annual,
  lag.in = NULL,
  number.iterations = 100,
  confidence.level = 0.95
)

placebo_control <- placebo_test( # run placebo tests for pretrends
  ct_control_match,
  boix_annual,
  lag.in = NULL,
  number.iterations = 100,
  confidence.level = 0.95
)

p <- plot_pretrends(placebo_base, placebo_control, bquote("Democracy on" ~ italic("Crisis Tally")))
ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/panel_match/ct_pretrends.png",
       device = png, bg = "white", width = 4.88, height = 3.34, units = "in")

#########################
# plot pretrends for external debt 
#########################

placebo_base <- placebo_test( # run placebo tests for pretrends
  debt_match_base,
  boix_annual,
  lag.in = NULL,
  number.iterations = 100,
  confidence.level = 0.95
)

placebo_control <- placebo_test( # run placebo tests for pretrends
  debt_match_controls,
  boix_annual,
  lag.in = NULL,
  number.iterations = 100,
  confidence.level = 0.95
)

p <- plot_pretrends(placebo_base, placebo_control, bquote("Democracy on" ~ italic("External Debt")))

ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/panel_match/debt_pretrends.png",
       device = png, bg = "white", width = 4.88, height = 3.34, units = "in")

#########################
# plot pretrends for banking 
#########################

placebo_base <- placebo_test( 
  banking_match_base,
  boix_annual,
  lag.in = NULL,
  number.iterations = 100,
  confidence.level = 0.95
)

placebo_control <- placebo_test( 
  banking_match_control,
  boix_annual,
  lag.in = NULL,
  number.iterations = 100,
  confidence.level = 0.95
)

p <- plot_pretrends(placebo_base, placebo_control, bquote("Democracy on" ~ italic("Banking Crisis")))
ggsave(plot = p, filename = "~/Dropbox/Apps/Overleaf/Writing Sample/panel_match/bank_pretrends.png",
       device = png, bg = "white", width = 4.88, height = 3.34, units = "in")

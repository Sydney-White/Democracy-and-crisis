# baseline panels for democratization and financial crisis 

library(tidyverse)
library(fixest)
library(texreg)
rm(list = ls()) 

# load data ---------------------------------------------------------------

boix_annual <- read.csv("Data/Data Output/boix_rr.csv") # annual data
boix_five <- read.csv("Data/Data Output/boix_five.csv")
boix_ten <- read.csv("Data/Data Output/boix_ten.csv")

# text ------------------------------------------------------------------

fe_rows = list("Period Length" = c("Annual", "Annual", "Five-year", "Five-year", "Ten-year", "Ten-year"),
               "Country FEs" = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "
                                 $\\checkmark$", "$\\checkmark$", "$\\checkmark$"),
               "Period FEs" =  c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
                                 "$\\checkmark$", "$\\checkmark$", "$\\checkmark$"))

coef_names = list("lag(democracy, 1)" = "Democracy$_{t-1}$", 
                  "lag(gdppc, 1)" = "GDP per capita$_{t-1}$", 
                  "lag(pop, 1)" = "Population$_{t-1}$", 
                  "lag(percent_growth, 1)" = "\\% Growth$_{t-1}$", 
                  "average_gdppc" = "Avg. GDP per capita", 
                  "average_pop" = "Avg. Pop", 
                  "average_growth" = "Avg. % Growth",
                  "intercept" = "Constant")

#############################
# regresses crisis tally outcome on lagged democracy, 
# with period and country fixed effects
#############################

base_annual <- feols(crisis_tally ~ lag(democracy, 1) | cown + year, 
              cluster=c("cown"), 
              data=boix_annual)
summary(base_annual)

res_base_annual <- round(unname(base_annual$coefficients[1]),3)

control_annual <- feols(crisis_tally ~ lag(democracy, 1) + lag(gdppc, 1) + 
                          lag(pop, 1) + lag(percent_growth,1) | cown + year, 
              cluster=c("cown"), 
              data=boix_annual)
summary(control_annual)
base_five <- feols(average_crisis_tally ~ lag(democracy, 1) | cown + five_year_period,
              cluster=c("cown"), 
              data=boix_five)

control_five <- feols(average_crisis_tally ~ lag(democracy, 1) + average_gdppc + average_pop + average_growth
                      | cown + five_year_period,
                      cluster=c("cown"), 
                      data=boix_five)

base_ten <- feols(average_crisis_tally ~ lag(democracy, 1) | cown + ten_year_period,
              cluster=c("cown"), 
              data=boix_ten)

control_ten <- feols(average_crisis_tally ~ lag(democracy, 1) + average_gdppc + average_pop + average_growth | cown + ten_year_period,
                  cluster=c("cown"), 
                  data=boix_ten)

texreg(list(base_annual, control_annual, base_five, control_five, base_ten, control_ten),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
       file = "~/Dropbox/Apps/Overleaf/Writing Sample/appendix/crisis_tally.tex",
       label = "table:financial_crisis_tally",
       custom.header = list("Dependent variable: \\textit{Crisis Tally}" = 1:6),
       custom.coef.map = coef_names,
       custom.gof.rows = fe_rows,
       custom.gof.names = c("N", "$R^2$"),
       stars = c(0.1, 0.05, 0.01),
       digits = 3,
       booktabs = T,
       threeparttable = T,
       use.packages = F,
       include.nobs = TRUE,
       include.groups = FALSE,
       include.rsquared = TRUE,
       include.adjrs = FALSE,
       include.proj.stats = FALSE,
       include.deviance = FALSE,
       include.loglik = FALSE,
       include.pseudors = FALSE,
       caption = "Effect of democracy on \\textit{Crisis Tally} at the country-level.", 
       custom.note = "\\parbox{\\linewidth}{This table shows effect of democracy on \\textit{Crisis Tally} for 
       70 states between 1800-2016. 
       All independent variables are lagged by one period.
       Model (1) is annual baseline. 
       Model (2) is annual with controls. 
       Model (3) is five-year baseline. 
       Model (4) is five-year with controls. 
       Model (5) is ten-year baseline. 
       Model (6) is ten-year with controls. 
       Period and country fixed effects are included.
       Robust standard errors are clustered at the country level and are shown in parantheses. 
       %stars.}")

#############################
# regresses banking crisis outcome on lagged democracy, 
# with period and country fixed effects
#############################

base_annual <- feols(banking_crisis ~ lag(democracy, 1)  | cown + year, 
                     cluster=c("cown"), 
                     data=boix_annual)

control_annual <- feols(banking_crisis ~ lag(democracy, 1) + 
                          lag(gdppc, 1) + 
                          lag(pop, 1) + lag(percent_growth, 1) | cown + year, 
                        cluster=c("cown"), 
                        data=boix_annual)

base_five <- feols(average_banking_crisis ~ lag(democracy, 1) | cown + five_year_period,
                   cluster=c("cown"), 
                   data=boix_five)

control_five <- feols(average_banking_crisis ~ lag(democracy, 1) + average_gdppc + average_pop + average_growth
                      | cown + five_year_period,
                      cluster=c("cown"), 
                      data=boix_five)

base_ten <- feols(average_banking_crisis ~ lag(democracy, 1) | cown + ten_year_period,
                  cluster=c("cown"), 
                  data=boix_ten)

control_ten <- feols(average_banking_crisis ~ lag(democracy, 1) + average_gdppc + average_pop + average_growth | cown + ten_year_period,
                     cluster=c("cown"), 
                     data=boix_ten)

texreg(list(base_annual, control_annual, base_five, control_five, base_ten, control_ten),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
       file = "~/Dropbox/Apps/Overleaf/Writing Sample/appendix/banking.tex",
       label = "table_banking",
       custom.header = list("Dependent variable: \\textit{Banking Crisis}" = 1:6),
       custom.coef.map = coef_names,
       custom.gof.rows = fe_rows,
       custom.gof.names = c("N", "$R^2$"),
       stars = c(0.1, 0.05, 0.01),
       digits = 3,
       booktabs = T,
       threeparttable = T,
       use.packages = F,
       include.nobs = TRUE,
       include.groups = FALSE,
       include.rsquared = TRUE,
       include.adjrs = FALSE,
       include.proj.stats = FALSE,
       include.deviance = FALSE,
       include.loglik = FALSE,
       include.pseudors = FALSE,
       caption = "Effect of democracy on \\textit{Banking Crisis} at the country-level.", 
       custom.note = "\\parbox{\\linewidth}{This table shows the effect ofdemocracy on \\textit{Banking Crisis} for
       70 states between 1800-2016. 
       All independent variables are lagged by one period.
       Model (1) is annual baseline. 
       Model (2) is annual with controls. 
       Model (3) is five-year baseline. 
       Model (4) is five-year with controls. 
       Model (5) is ten-year baseline. 
       Model (6) is ten-year with controls.
       Period and country fixed effects are included. 
       Robust standard errors clustered at the country level are shown in parantheses. 
       %stars.}")

#############################
# regresses banking crisis outcome on lagged democracy, 
# with period and country fixed effects
#############################

base_annual <- feols(external_default_in_1975 ~ lag(democracy, 1) | cown + year, 
                     cluster=c("cown"), 
                     data=boix_annual)

control_annual <- feols(external_default_in_1975 ~ lag(democracy, 1) + lag(gdppc, 1) + 
                          lag(pop, 1) + lag(percent_growth, 1)
                        | cown + year, 
                        cluster=c("cown"), 
                        data=boix_annual)

summary(control_annual)
base_five <- feols(average_external_debt_crisis ~ lag(democracy, 1) | cown + five_year_period,
                   cluster=c("cown"), 
                   data=boix_five)

control_five <- feols(average_external_debt_crisis ~ lag(democracy, 1) + average_gdppc + average_pop + average_growth 
                      | cown + five_year_period,
                      cluster=c("cown"), 
                      data=boix_five)

base_ten <- feols(average_external_debt_crisis ~ lag(democracy, 1) | cown + ten_year_period,
                  cluster=c("cown"), 
                  data=boix_ten)

control_ten <- feols(average_external_debt_crisis ~ lag(democracy, 1) + average_gdppc + average_pop + average_growth | cown + ten_year_period,
                     cluster=c("cown"), 
                     data=boix_ten)

texreg(list(base_annual, control_annual, base_five, control_five, base_ten, control_ten),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
       file = "~/Dropbox/Apps/Overleaf/Writing Sample/appendix/external_debt.tex",
       label = "fig:external_debt",
       custom.coef.map = coef_names,
       custom.gof.rows = fe_rows,
       custom.gof.names = c("N", "$R^2$"),
       stars = c(0.1, 0.05, 0.01),
       digits = 3,
       booktabs = T,
       threeparttable = T,
       use.packages = F,
       include.nobs = TRUE,
       include.groups = FALSE,
       include.rsquared = TRUE,
       include.adjrs = FALSE,
       include.proj.stats = FALSE,
       include.deviance = FALSE,
       include.loglik = FALSE,
       include.pseudors = FALSE,
       custom.header = list("Dependent variable: \\textit{External Debt}" = 1:6),
       caption = "Effect of democracy on \\textit{External Debt} at the country-level.",
       custom.note = "\\parbox{\\linewidth}{This table shows the effect of democracy on the \\textit{External Debt} for 
       70 nation-states between 1800-2016. 
       All independent variables are lagged by one period. 
       Model (1) is annual baseline. 
       Model (2) is annual with controls. 
       Model (3) is five-year baseline. 
       Model (4) is five-year with controls. 
       Model (5) is ten-year baseline. 
       Model (6) is ten-year with controls.
       Robust standard errors are clustered at the country level. 
       Period and country fixed effects are included. %stars.}")

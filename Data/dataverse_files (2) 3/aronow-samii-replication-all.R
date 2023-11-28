rm(list=ls())
# change wd as needed:
library(maptools)
library(foreign)
library(sf)

# Jensen example

# Files needed:
# 5603 folder with world shapefile and auxiliary files
# jensen-rep.dta
# mapnames_filled.csv

world <-  read_sf("Data/dataverse_files (2) 3/world_countries_boundary_file_world_2002.shp")
jensen.cc <- read.dta("Data/dataverse_files (2) 3/jensen-rep.dta") 

X.vars <- c(	"var5",
				"market",
				"lgdppc",
				"gdpgrowt",
				"tradeofg",
				"overallb",
				"generalg",
				"country",
				"d2",
				"d3")

X.vars.f <- paste(X.vars,collapse="+") # Fvar4 must be FDI, right? so this is the outcome, regime is the 
# "treatment"

fit.y <- lm(as.formula(paste("Fvar5~regime+", X.vars.f, sep="")), data=jensen.cc)
fit.d <- lm(as.formula(paste("regime~", X.vars.f, sep="")), data=jensen.cc)
d.tilde <- as.numeric(residuals(fit.d))
w <- d.tilde^2

w1 <- tapply(w, jensen.cc$country, mean)
mapnames <-  read.csv("Data/dataverse_files (2) 3/mapnames_filled.csv")
mapnames$weight <- 0
mapnames$weight[match(rownames(w1), mapnames$jensen)] <- as.numeric(w1)

attributes(world)$data$weight <- 0
attributes(world)$data$weight[match(mapnames$mapname,attributes(world)$data$NAME)] <- mapnames$weight

attributes(world)$data$incl <- 0
attributes(world)$data$incl[match(mapnames$mapname,attributes(world)$data$NAME)] <- as.numeric(!is.na(mapnames$jensen))

# Sample

plot(	world, 
		col=gray(1-.75*attributes(world)$data$incl),
		border="gray", lwd=.25)

# Effective sample

plot(world, col=gray(1-abs(attributes(world)$data$weight)/max(abs(attributes(world)$data$weight))),
		border="gray", lwd=.25)

# Gerber and Huber example

rm(list=ls())
# change wd as needed 
setwd("~/Dropbox/Whose Effect/data/replication")

library(foreign)
library(xtable)

# Files needed:
# gerber-huber-analysis-data.dta

gerb <- read.dta("gerber-huber-analysis-data.dta")

fit <- lm(delta_vacationspend~pre_pid5
								+C_age
								+C_age2
								+C_female
								+C_hispanic
								+C_black
								+C_union
								+C_income
								+C_incomedkna
								+C_educ, data=gerb)

vars.all <- attributes(gerb)$names
labs.all <- attributes(gerb)$var.labels
vars <- c("pre_pid5",
			"C_age",
			"C_female",
			"C_hispanic",
			"C_black",
			"C_union",
			"C_income",
			"C_incomedkna",
			"C_educ",
			"pre_hhincomeforecast",
			"post_hhincomeforecast",
			"pre_nationaleconforecast",
			"post_nationaleconforecast",
			"delta_holidayspend",
			"delta_vacationspend",
			"pre_happy",
			"post_happy",
			"pre_stateeconforecast",
			"post_stateeconforecast")
labs <- labs.all[match(vars, vars.all)]

# summary statistics
wt <- gerb$wt
meannum <- function(x) mean(as.numeric(x), na.rm=T)
sdnum <- function(x) (mean(as.numeric(x)^2, na.rm=T) - mean(as.numeric(x), na.rm=T)^2)^.5
wtmeannum <- function(x) weighted.mean(as.numeric(x),wt, na.rm=T)
wtmeannum2 <- function(x) weighted.mean(as.numeric(x),gerb$weight, na.rm=T)
wtsdnum <- function(x) (weighted.mean(as.numeric(x)^2,wt, na.rm=T) - weighted.mean(as.numeric(x),wt, na.rm=T)^2)^.5
minnum <- function(x) min(as.numeric(x), na.rm=T)
maxnum <- function(x) max(as.numeric(x), na.rm=T)

nomsampmean <- apply(gerb[,vars], 2, meannum)
nomsampsd <- apply(gerb[,vars], 2, sdnum)
effsampmean <- apply(gerb[,vars], 2, wtmeannum)
effsampsd <- apply(gerb[,vars], 2, wtsdnum)

sumstats <- round(100*cbind(nomsampmean, 
					nomsampsd,
					effsampmean,
					effsampsd))/100
rownames(sumstats) <- labs
colnames(sumstats) <- c("Mean",
						"S.D.",
						"Mean",
						"S.D.")
sink(file="gerber-huber-table.tex")
xtable(sumstats)
sink()
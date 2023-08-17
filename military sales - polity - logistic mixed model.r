
#.libPaths("C:/Users/hoend008/AppData/Local/R/library")

# Load packages
# library(ggplot2)
library(lme4)
library(readxl)
library(car)
#library(glmmTMB)

#library(nlme)

# Import data
setwd("C:/Analysis/Information war")
#setwd("C:/Users/hoend008/OneDrive - WageningenUR/Book datasets/Chapter 4/Excels/Military aid/SIPRI")
dep <- read_excel("SIPRI_polity_2022-12-16.xlsx")
dep <- as.data.frame(dep)
# attach(dep)
# Check data
head(dep)
summary(dep)

# -------------------------------------------------- Data exploration

# change numeric to factor
dep$nato = factor(dep$nato)
dep$ex_soviet_state = factor(dep$ex_soviet_state)
dep$bought_weapons = factor(dep$bought_weapons)

# change polity into a more easier to interpret scale
dep$polity_democracy_index = dep$polity_democracy_index + 11


# ------------ Plot: Histogram of dependent variable: military sales
hist(dep$value)
# try log
dep$value_log <- log(dep$value+1)
hist(dep$value_log)

# ------------ Plot: histogram of polity_democracy_index
hist(dep$polity_democracy_index)

# ------------ Plot: predictor variable: military spending
#hist(dep$military_spending_spline)
# try log
#dep$military_spending_spline_log <- log(dep$military_spending_spline+1)
#hist(dep$military_spending_spline_log)

# transform year so it starts from 0
dep$year_from_zero = dep$year - min(dep$year)
dep$year_from_zero_sqrt = sqrt(dep$year_from_zero)

#dep$wait_for_nato = ifelse(is.na(dep$entrynato), 2022-1949, dep$entrynato - 1949)
#dep$wait_for_nato = sqrt(dep$wait_for_nato)


lmer_logistic_main <- function(){

	#dep$buy <- factor(ifelse(dep$value > 0, "2. buy", "1. not buy"))
	table(dep$bought_weapons)
	
	#select where military spendig is not missing
	dep <- dep[!is.na(dep$military_spending_value_dollars),]
	dim(dep)
	table(dep$bought_weapons)
	
	# Unconditional Means Model
	model.1 <- glmer(bought_weapons~ 1 + (1 | code3), 
						data = dep, 
						family=binomial(link='logit'))
	summary(model.1)
	
	# Unconditional growth model (random intercepts)
	model.2 <- glmer(bought_weapons~ 1 + year_from_zero + (1 | code3), 
						data = dep, 
						family=binomial(link='logit'),
						nAGQ=0)
	summary(model.2)
	
	# Unconditional growth model (random intercepts & slopes)
	model.3 <- glmer(bought_weapons~ 1 + year_from_zero + (1 + year_from_zero | code3), 
						data = dep, 
						family=binomial(link='logit'),
						nAGQ=0)
	summary(model.3)
	
	# random intercepts & slopes, effect of nato
	#model.4 <- glmer(bought_weapons~ 1 + year_from_zero*nato + (1 + year_from_zero | code3), 
	#					data = dep, 
	#					family=binomial(link='logit'))
	#summary(model.4)

	model.4 <- glmer(bought_weapons~ 1 + year_from_zero + nato + (1 + year_from_zero | code3), 
						data = dep, 
						family=binomial(link='logit'),
						nAGQ=0)
	summary(model.4)
	
	#anova(model.4a, model.4)
	
	# random intercepts & slopes, effect of nato and ex_soviet_state
	model.5 <- glmer(bought_weapons~ 1 + year_from_zero + nato + ex_soviet_state + (1 + year_from_zero | code3), 
						data = dep, 
						family=binomial(link='logit'),
						nAGQ=0)
	summary(model.5)

	
	# random intercepts & slopes, effect of nato, ex_soviet_state and military_spending
	model.6 <- glmer(bought_weapons~ 1 + year_from_zero + nato + ex_soviet_state + log(military_spending_value_dollars+1) + (1 + year_from_zero | code3), 
						data = dep, 
						family=binomial(link='logit'),
						nAGQ=0)
	summary(model.6)

	# random intercepts & slopes, effect of nato, ex_soviet_state and military_spending and regime type
	model.7 <- glmer(bought_weapons~ 1 + year_from_zero + nato + ex_soviet_state + log(military_spending_value_dollars+1) + polity_democracy_index + (1 + year_from_zero | code3), 
						data = dep, 
						family=binomial(link='logit'),
						nAGQ=0)
	summary(model.7)
	
	# compare models with and without regime type
	anova(model.2, model.3, model.4, model.5, model.6, model.7)
	
}
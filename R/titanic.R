##########################################################################
#name:titanic.R
#author: Sarah Bainn
#description: Analysis on titanic dataset
#date last updated:05/16/2020
##########################################################################

library(MASS)
library(readstata13)
library(tidyverse)
library(estimatr)
library(sjPlot)
#read data and generate dummies
Titanic <- read.dta13("/cloud/project/Data/titanic.dta")
Titanic$female <- ifelse(Titanic$sex == "women", 1, 0)
Titanic$firstclass <- ifelse(Titanic$class == "1st class", 1, 0)
Titanic$survival <- ifelse(Titanic$survived == "yes", 1, 0)
Titanic$child <- ifelse(Titanic$age == "child", 1, 0)

#robust and non robust short regressions
attach(Titanic)
lm_short <- lm(survival~firstclass)
lmshort_robust <- rlm(survival~firstclass, data = Titanic, psi = psi.hampel)

#calculating ovb for sex and age 
sex_ovb = cov(Titanic$female, Titanic$firstclass) / var(Titanic$firstclass)
age_ovb = cov(Titanic$child, Titanic$firstclass) / var(Titanic$firstclass)

#finding true effect
true_eff = lm_short$coefficients[2]
true_eff = true_eff - sex_ovb
true_eff = true_eff - age_ovb
lm_short$coefficients[2]

#long regression
lm_long <- lm(survival~firstclass+female+child)
lmlong_robust <- rlm(survival~firstclass+female+child, data = Titanic, psi = psi.huber)
lm_long$coefficients[2]
lmlong_robust$coefficients[2]

###beautiful table
tab_model(lm_short,lmshort_robust, lm_long, lmlong_robust, collapse.se = "TRUE", file = "test.html")

#auxiliary regression
reg_long <- lm(survival~firstclass+female+child)
aux_reg = lm(firstclass ~ child + female)
Titanic$residuals = aux_reg$residuals
res_reg = lm(survival ~ Titanic$residuals)
reg_long$coefficients[2]
res_reg$coefficients[2]

#beautiful table
tab_model(reg_long, aux_reg, res_reg, collapse.se = "TRUE", file = "test.html")


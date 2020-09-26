################################################################################
# PROGRAM: 
# PURPOSE:
# WRITTEN BY: Phillip Hungerford
# DATE:
# DATA REQUIRED:
################################################################################
# libraries
library(tidyverse)
library(haven)
library(ggplot2)

options(scipen = 999)
################################################################################
# load data
faculty <- read_dta(file = "../data/faculty.dta")

# salary: academic year (9-month) salary in U.S. dollars
# male: gender (1 = male; 0 = female)
# market: marketability of academic discipline, defined as the ratio of the 
#         national average salary paid in the discipline to the national average 
#         across disciplines
# yearsdg: time since degree
# rank: academic rank (1 = assistant professor; 2 = associate professor;
##      3 = full professor)

# check
str(faculty)
################################################################################
# Summary
#===============================================================================
# table summary
faculty %>%
  group_by(male) %>% 
  summarise(
    mean = mean(salary),
    sd = sd(salary),
    N = n())

# summarise specific variables, not all
faculty %>% summarise(mean = mean(salary), sd = sd(salary), N = n())
#===============================================================================
# 1.3. Independent-samples t test.
#===============================================================================
# Figure 1.1: Box plots of salary and log salary by gender
# box plot
boxplot(salary ~ male, data= faculty)

# box plot of log salary
boxplot(log(salary) ~ male, data= faculty)

# histogram
ggplot(faculty,aes(x=salary)) + geom_histogram() + facet_grid(~male)
ggplot(faculty,aes(x=log(salary))) + geom_histogram() + facet_grid(~male)

#===============================================================================
# t-test with equal variances
t.test(salary ~ male, data= faculty, var.equal = TRUE)

# t-test with unequal variances
t.test(salary ~ male, data= faculty, var.equal = FALSE)
#===============================================================================
# 1.4. ANOVA

# Compute the one-way analysis of variance
res.aov <- aov(salary ~ male, data= faculty)

# Summary of the analysis
summary(res.aov) # F(1, 512) = 76.96, p < 0.001

# MSE from model 
mse <- 139887048

# calculate estimated root population variance
sqrt(mse)


m2 <- lm(salary ~ male, data= faculty)
summary(m2)
#===============================================================================
#1.5. Simple linear regression

################################################################################
##################################### END ######################################
################################################################################
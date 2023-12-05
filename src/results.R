#############################################################
## BIS 567 Final Project
## Authors: Eugene Han, Elder Veliz
##
## The purpose of this script is to analyze and visualize
## the results of the MCMC sampling
#############################################################

################################
# 0 - Load libraries
################################

library(tidyverse)
library(rjags)
library(coda)


################################
# 1 - Fixed effects
################################

# Load outputs
load("data/results_base.RData")
load("data/results_mixed_1.RData")
load("data/results_mixed_2.RData")

# Trace + ACF plots for fixed effects
# Recall that 2011 is reference year
# Trace plots look great for all fixed effects for all 3 models
# Year 2012
traceplot(results_base[, "beta[1]", drop = FALSE], main = "Trace of year_2012, Base")
traceplot(results_mixed_1[, "beta[1]", drop = FALSE], main = "Trace of year_2012, School Effects")
traceplot(results_mixed_2[, "beta[1]", drop = FALSE], main = "Trace of year_2012, School + County Effects")
autocorr.plot(results_base[, "beta[1]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_1[, "beta[1]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_2[, "beta[1]", drop = FALSE], ask = FALSE)

# Year 2013
traceplot(results_base[, "beta[2]", drop = FALSE], main = "Trace of year_2013, Base")
traceplot(results_mixed_1[, "beta[2]", drop = FALSE], main = "Trace of year_2013, School Effects")
traceplot(results_mixed_2[, "beta[2]", drop = FALSE], main = "Trace of year_2013, School + County Effects")
autocorr.plot(results_base[, "beta[2]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_1[, "beta[2]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_2[, "beta[2]", drop = FALSE], ask = FALSE)

# Year 2014
traceplot(results_base[, "beta[3]", drop = FALSE], main = "Trace of year_2014, Base")
traceplot(results_mixed_1[, "beta[3]", drop = FALSE], main = "Trace of year_2014, School Effects")
traceplot(results_mixed_2[, "beta[3]", drop = FALSE], main = "Trace of year_2014, School + County Effects")
autocorr.plot(results_base[, "beta[3]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_1[, "beta[3]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_2[, "beta[3]", drop = FALSE], ask = FALSE)

# Year 2015
traceplot(results_base[, "beta[4]", drop = FALSE], main = "Trace of year_2015, Base")
traceplot(results_mixed_1[, "beta[4]", drop = FALSE], main = "Trace of year_2015, School Effects")
traceplot(results_mixed_2[, "beta[4]", drop = FALSE], main = "Trace of year_2015, School + County Effects")
autocorr.plot(results_base[, "beta[4]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_1[, "beta[4]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_2[, "beta[4]", drop = FALSE], ask = FALSE)

# Year 2016
traceplot(results_base[, "beta[5]", drop = FALSE], main = "Trace of year_2016, Base")
traceplot(results_mixed_1[, "beta[5]", drop = FALSE], main = "Trace of year_2016, School Effects")
traceplot(results_mixed_2[, "beta[5]", drop = FALSE], main = "Trace of year_2016, School + County Effects")
autocorr.plot(results_base[, "beta[5]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_1[, "beta[5]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_2[, "beta[5]", drop = FALSE], ask = FALSE)

# Year 2017
traceplot(results_base[, "beta[6]", drop = FALSE], main = "Trace of year_2017, Base")
traceplot(results_mixed_1[, "beta[6]", drop = FALSE], main = "Trace of year_2017, School Effects")
traceplot(results_mixed_2[, "beta[6]", drop = FALSE], main = "Trace of year_2017, School + County Effects")
autocorr.plot(results_base[, "beta[6]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_1[, "beta[6]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_2[, "beta[6]", drop = FALSE], ask = FALSE)

# Title I Eligibility
traceplot(results_base[, "beta[7]", drop = FALSE], main = "Trace of title_i_eligibility, Base")
traceplot(results_mixed_1[, "beta[7]", drop = FALSE], main = "Trace of title_i_eligibility, School Effects")
traceplot(results_mixed_2[, "beta[7]", drop = FALSE], main = "Trace of title_i_eligibility, School + County Effects")
autocorr.plot(results_base[, "beta[7]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_1[, "beta[7]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_2[, "beta[7]", drop = FALSE], ask = FALSE)

# Student to Teacher Ratio
traceplot(results_base[, "beta[8]", drop = FALSE], main = "Trace of student_teacher_ratio, Base")
traceplot(results_mixed_1[, "beta[8]", drop = FALSE], main = "Trace of student_teacher_ratio, School Effects")
traceplot(results_mixed_2[, "beta[8]", drop = FALSE], main = "Trace of student_teacher_ratio, School + County Effects")
autocorr.plot(results_base[, "beta[8]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_1[, "beta[8]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_2[, "beta[8]", drop = FALSE], ask = FALSE)


# Summary of posterior distributions
fixed_effects <- c("beta[1]", "beta[2]", "beta[3]", "beta[4]", 
                   "beta[5]", "beta[6]", "beta[7]", "beta[8]")
summary(results_base[, fixed_effects, drop = FALSE])
summary(results_mixed_1[, fixed_effects, drop = FALSE])
summary(results_mixed_2[, fixed_effects, drop = FALSE])


# Effective sample sizes
effectiveSize(results_base[, fixed_effects, drop = FALSE])
effectiveSize(results_mixed_1[, fixed_effects, drop = FALSE])
effectiveSize(results_mixed_2[, fixed_effects, drop = FALSE])


# Gelman-Rubin diagnostic since we are using multiple chains
gelman.diag(results_base[, fixed_effects, drop = FALSE])
gelman.diag(results_mixed_1[, fixed_effects, drop = FALSE])
gelman.diag(results_mixed_2[, fixed_effects, drop = FALSE])


################################
# 1 - School random effects
################################

# TODO: choose which schools to show trace plots for
# and compare distributions between the two mixed effects models

# Note: there are 825 unique schools
traceplot(results_mixed_2[, "alpha[42]", drop = FALSE])
test <- sprintf("alpha[%d]", 401:600)

# 6, 14, 18, 20, 21, 22, 23, 25, 26, 27, 28, 29, 30, 41, 48, 49, 55, 56, 57, 59, 61, 62, 69
candidates <- c(6, 14, 18, 20, 21, 22, 23, 25, 26, 27, 28, 29, 30, 41, 48, 49, 55, 56, 57, 59, 61, 62, 69)
school_indices <- c(360000800193, 360002101704, 360007600624, 360007603352, 360007603680, 360007605621, 360007700116
,360007700585
,360007700595
,360007700637
,360007700649
,360007700691
,360007700692
,360007702871
,360007705084
,360007705085
,360007705622
,360007705624
,360007705625
,360007705764
,360007705770
,360007705771
,360007805113)

################################
# 2 - County random effects
################################

# TODO: choose which counties to show trace plots for

# Note: there are 61 unique counties


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


# Geweke diagnostic scores, arbitrarily chose chain #1 (out of 3)
geweke.diag(results_base[, fixed_effects, drop = FALSE])[1]
geweke.diag(results_mixed_1[, fixed_effects, drop = FALSE])[1]
geweke.diag(results_mixed_2[, fixed_effects, drop = FALSE])[1]


################################
# 1 - School random effects
################################

# TODO: choose which schools to show trace plots for
# and compare distributions between the two mixed effects models


################################
# 2 - County random effects
################################

# TODO: choose which counties to show trace plots for

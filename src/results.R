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

# Intercept
traceplot(results_base[, "beta[9]", drop = FALSE], main = "Trace of intercept, Base")
traceplot(results_mixed_1[, "beta[9]", drop = FALSE], main = "Trace of intercept, School Effects")
traceplot(results_mixed_2[, "beta[9]", drop = FALSE], main = "Trace of intercept, School + County Effects")
autocorr.plot(results_base[, "beta[9]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_1[, "beta[9]", drop = FALSE], ask = FALSE)
autocorr.plot(results_mixed_2[, "beta[9]", drop = FALSE], ask = FALSE)


# Summary of posterior distributions
fixed_effects <- c("beta[1]", "beta[2]", "beta[3]", "beta[4]", 
                   "beta[5]", "beta[6]", "beta[7]", "beta[8]", "beta[9]")
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
combined_samples <- do.call(rbind, results_mixed_1)

# Prepare a matrix to store the quantiles and the indicator
alpha_quantiles <- matrix(NA, nrow = 825, ncol = 4)
colnames(alpha_quantiles) <- c("lwr", "upr", "Zero", "Index")

# Loop over each alpha parameter and calculate quantiles and the indicator
for (i in 1:825) {
  # Extract samples for this specific alpha
  alpha_samples <- combined_samples[, sprintf("alpha[%d]", i)]
  
  # Calculate the 2.5% and 97.5% quantiles
  quantiles <- quantile(alpha_samples, probs = c(0.025, 0.975))
  
  # Check if 0 is in the interval
  zero_in_interval <- as.numeric(quantiles["2.5%"] < 0 & quantiles["97.5%"] > 0)
  
  # Store the quantiles and the indicator
  alpha_quantiles[i, ] <- c(quantiles["2.5%"], quantiles["97.5%"], zero_in_interval, i)
}
#identify greatest mean magnitudes 
as.data.frame(alpha_quantiles) %>% filter(Zero == 0) %>% mutate(absmean = abs((upr + lwr) / 2)) %>% arrange(-absmean)

# BRONX COLLEGIATE ACADEMY (Bronx County): highest mean aphi
ny %>% filter(ncessch == 360008605669)
traceplot(results_mixed_1[, "alpha[111]", drop = FALSE], main = "")

# MATTITUCK JUNIOR-SENIOR HIGH SCHOOL (Suffolk County): lowest mean aphi
ny %>% filter(ncessch == 360002101704)
traceplot(results_mixed_1[, "alpha[14]", drop = FALSE], main = "")

# Stuyvesant (New York County): aphi = 0, recognizable school
ny %>% filter(ncessch == 360007702877)
traceplot(results_mixed_1[, "alpha[42]", drop = FALSE], main = "")


################################
# 2 - County random effects
################################

# Note: there are 61 unique counties (of 62 actual)

# County phi values were mapped under `county_map.R`
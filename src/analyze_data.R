#############################################################
## BIS 567 Final Project
## Authors: Eugene Han, Elder Veliz
##
## The purpose of this script is to perform a Bayesian
## analysis of our dataset.
#############################################################

################################
# 0 - Load libraries
################################

library(tidyverse)
library(rjags)
library(reshape2)
library(coda)
library(readr)

################################
# 1 - Pre-modeling preparation
################################

# Load in data
load("data/final_data.RData")

# Set seed
set.seed(567)


# # PLOT math proficiency over time
# ggplot(ny, aes(x = year, y = math_test_pct_prof_midpt, group = ncessch, color = as.factor(ncessch))) +
#   geom_line() +
#   labs(title = "Math Test Proficiency Midpoint by Year and School",
#        x = "Year",
#        y = "Math Test Proficiency Midpoint",
#        color = "School ID") +
#   theme_minimal() + 
#   theme(legend.position = "none")


# Create indices for schools, counties, years
unique_ids <- unique(ny$ncessch)
id_mapping <- setNames(seq_along(unique_ids), unique_ids)
ny$school_index <- rep(NA,nrow(ny))
for (i in 1:nrow(ny)) {
  ny$school_index[i] <- id_mapping[as.character(ny$ncessch[i])]
}

ny$county_index <- as.numeric(as.factor(ny$county))
ny$year_index <- as.numeric(as.factor(ny$year))

# select relevant columns
ny_jags <- ny[c("year", "logit_math_midpt", "title_i_eligible", "student_teacher_ratio", "school_index", "county_index", "year_index")]

# Reshape Y to be matrix
Y_matrix <- as.matrix(dcast(ny, school_index ~ year, value.var = "logit_math_midpt")[,-1])

N_schools <- length(unique(ny_jags$school_index))
N_years <- length(unique(ny_jags$year))
N_counties <- length(unique(ny_jags$county_index))

# One-hot encoding for years
for (year in unique(ny_jags$year)) {
  ny_jags[paste("year", year, sep = "_")] <- ifelse(ny_jags$year == year, 1, 0)
}

N_predictors <- 2 + N_years # title_i_eligible, student_teacher_ratio, and years

# Update X_array dimensions
X_array <- array(dim = c(N_schools, N_years, N_predictors))

# Fill the array with new predictors
for (school in unique(ny_jags$school_index)) {
  for (year in unique(ny_jags$year_index)) {
    school_rows <- ny_jags[ny_jags$school_index == school & ny_jags$year_index == year, ]
    if (nrow(school_rows) > 0) {
      X_array[school, year, 1:N_years] <- as.numeric(school_rows[3:(2+N_years)]) # One-hot encoded years
      X_array[school, year, (N_years+1)] <- school_rows[["title_i_eligible"]]
      X_array[school, year, (N_years+2)] <- school_rows[["student_teacher_ratio"]]
    }
  }
}


################################
# 2 - Formulate model and MCMC
################################

# Read in model strings
model_string_base <- read_file("models/base_lm.txt")
model_string_mixed_1 <- read_file("models/mixed_with_school.txt")
model_string_mixed_2 <- read_file("models/mixed_with_school_and_county.txt")

# Initialize data list and initial values
jags_data_list <- list(
  Y = Y_matrix,
  X = X_array,
  county = ny_jags$county_index,
  N_schools = N_schools,
  N_counties = N_counties,
  N_years = N_years,
  N_beta = N_predictors, 
  school = ny_jags$school_index
)

initial_values <- list(
  beta = rnorm(N_predictors, 0, 0.01),  # Assuming 3 fixed effects: year, title_i_eligible, and student_teacher_ratio
  alpha = rep(0, N_schools),
  phi = rep(0, N_counties),
  tau2_alpha = 1, 
  tau2_phi = 1, 
  tau2_epsilon = 1
)

# Initialize models
model_base <- jags.model(textConnection(model_string_base), 
                         data = jags_data_list, 
                         inits = initial_values, 
                         n.chains = 3)
model_mixed_1 <- jags.model(textConnection(model_string_mixed_1), 
                            data = jags_data_list, 
                            inits = initial_values, 
                            n.chains = 3)
model_mixed_2 <- jags.model(textConnection(model_string_mixed_2), 
                            data = jags_data_list, 
                            inits = initial_values, 
                            n.chains = 3)


# Run MCMC and store results
results_base <- coda.samples(model = model_base, 
                             variable.names = c("beta", "sigma2_epsilon"), 
                             n.iter = 10000)
results_mixed_1 <- coda.samples(model = model_mixed_1, 
                                variable.names = c("beta", "alpha", "sigma2_epsilon", "sigma2_alpha"), 
                                n.iter = 10000)
results_mixed_2 <- coda.samples(model = model_mixed_2, 
                                variable.names = c("beta", "alpha", "phi", "sigma2_epsilon", "sigma2_alpha", "sigma2_phi"), 
                                n.iter = 10000)

# save(results_base, file = "data/results_base.RData")
# save(results_mixed_1, file = "data/results_mixed_1.RData")
# save(results_mixed_2, file = "data/results_mixed_2.RData")

################################
# 3 - Convergence diagnostics
################################

# Load in saved MCMC runs
# load("data/results_base.RData")
# load("data/results_mixed_1.RData")
# load("data/results_mixed_2.RData")


# traceplot(results[, "beta[9]", drop=FALSE]) # student_teacher_ratio - bad
# traceplot(results[, "beta[8]", drop=FALSE]) # title_i_eligible - bad
# traceplot(results[, "beta[7]", drop=FALSE]) #2017 - good
# traceplot(results[, "beta[6]", drop=FALSE]) #2016 - good
# traceplot(results[, "beta[5]", drop=FALSE]) #2015 - good
# traceplot(results[, "beta[4]", drop=FALSE]) #2014 - good
# traceplot(results[, "beta[3]", drop=FALSE]) #2013 - bad
# traceplot(results[, "beta[2]", drop=FALSE]) #2012 - bad
# traceplot(results[, "beta[1]", drop=FALSE]) #2011 - bad

# traceplot(results)
# autocorr.plot(results[,200])

# acf_results <- autocorr.diag(results)



################################
# 4 - Compare models
################################

# Compute DIC statistics
dic_stats_base <- dic.samples(model = model_base, n.iter = 10000, type = "pD")
dic_stats_mixed_1 <- dic.samples(model = model_mixed_1, n.iter = 10000, type = "pD")
dic_stats_mixed_2 <- dic.samples(model = model_mixed_2, n.iter = 10000, type = "pD")

dic_stats_base
dic_stats_mixed_1
dic_stats_mixed_2

  
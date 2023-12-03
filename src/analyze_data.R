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


# create indices for schools, counties, years
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

model_string <- "model {
  # Priors
  for (j in 1:N_beta) {
    beta[j] ~ dnorm(0, 0.0001)
  }
  
  tau2_alpha ~ dgamma(0.01, 0.01)
  tau2_phi ~ dgamma(0.01, 0.01)
  tau2 ~ dgamma(0.01, 0.01)

  # simulate Inverse Gamma
  sigma2_alpha <- 1 / tau2_alpha
  sigma2_phi <- 1 / tau2_phi
  sigma2 <- 1 / tau2
  
  # Random effects
  for (i in 1:N_schools) {
    alpha[i] ~ dnorm(0, sigma2_alpha) # school random effect
  }
  
  for (j in 1:N_counties) {
    phi[j] ~ dnorm(0, sigma2_phi)     # county random effect
  }
  
  # Likelihood
  for (i in 1:N_schools) {
    for (t in 1:N_years) {
      mu[i, t] <- inprod(X[i, t, ], beta) + alpha[school[i]] + phi[county[i]]
      Y[i, t] ~ dnorm(mu[i, t], sigma2)       # normal likelihood
    }
  }
}"



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
  tau2 = 1
)

jags_model <- jags.model(textConnection(model_string), data = jags_data_list, inits = initial_values, n.chains = 3)

results <- coda.samples(model = jags_model, variable.names = c("beta", "alpha", "phi", "sigma2_alpha", "sigma2_phi", "sigma2"), n.iter = 1000000)

save(results, file = "data/model_results.RData")

################################
# 3 - Convergence diagnostics
################################

traceplot(results[, "beta[9]", drop=FALSE]) # student_teacher_ratio - bad
traceplot(results[, "beta[8]", drop=FALSE]) # title_i_eligible - bad
traceplot(results[, "beta[7]", drop=FALSE]) #2017 - good
traceplot(results[, "beta[6]", drop=FALSE]) #2016 - good
traceplot(results[, "beta[5]", drop=FALSE]) #2015 - good
traceplot(results[, "beta[4]", drop=FALSE]) #2014 - good
traceplot(results[, "beta[3]", drop=FALSE]) #2013 - bad
traceplot(results[, "beta[2]", drop=FALSE]) #2012 - bad
traceplot(results[, "beta[1]", drop=FALSE]) #2011 - bad


################################
# 4 - Compare models
################################
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
N_predictors <- 3  # year, title_i_eligible, and student_teacher_ratio
N_counties <- length(unique(ny_jags$county_index))

# Reshape X
X_array <- array(dim = c(N_schools, N_years, N_predictors))

# Fill the array
for (school in unique(ny_jags$school_index)) {
  for (year in unique(ny_jags$year_index)) {
    school_rows <- ny_jags[ny_jags$school_index == school & ny_jags$year_index == year, ]
    if (nrow(school_rows) > 0) {
      X_array[school, year, 1] <- school_rows[["year"]]
      X_array[school, year, 2] <- school_rows[["title_i_eligible"]]
      X_array[school, year, 3] <- school_rows[["student_teacher_ratio"]]
    }
  }
}

################################
# 2 - Formulate model and MCMC
################################

model_string <- "model {
  # Likelihood
  for (i in 1:N_schools) {
    for (t in 1:N_years) {
      Y[i, t] ~ dnorm(mu[i, t], tau)
      mu[i, t] <- inprod(X[i, t,], beta) + alpha[i] + phi[county[i]]
    }
  }
  
  # Priors for fixed effects beta
  for (j in 1:N_beta) {
    beta[j] ~ dnorm(0, 0.0001)
  }
  
  # Random effects for schools
  for (s in 1:N_schools) {
    alpha[s] ~ dmnorm(alpha_zero[], cov_alpha[ , ])
    alpha_zero[s] <- 0  # Mean zero for multivariate normal
  }
  
  # Random effects for counties
  for (c in 1:N_counties) {
    phi[c] ~ dmnorm(phi_zero[], cov_phi[ , ])
    phi_zero[c] <- 0  # Mean zero for multivariate normal
  }
  
  # Covariance matrices and their hyperpriors
  cov_alpha[1:N_schools, 1:N_schools] ~ dwish(W_alpha[,], df_alpha)
  cov_phi[1:N_counties, 1:N_counties] ~ dwish(W_phi[,], df_phi)
  
  # Precision parameter tau and its prior
  tau ~ dgamma(a_tau, b_tau)
  sigma2 <- 1 / tau  # Variance
}"


jags_data_list <- list(
  Y = Y_matrix,
  X = X_array,
  county = ny_jags$county_index,
  N_schools = N_schools,
  N_counties = N_counties,
  N_years = N_years,
  N_beta = 3,  # Number of fixed effect predictors
  a_tau = 0.01,  # ADJUST based on assumptions
  b_tau = 0.01,  # ADJUST based on assumptions
  W_alpha = diag(rep(1, N_schools)),
  W_phi = diag(rep(1, N_counties)), 
  df_alpha = N_schools + 1,
  df_phi = N_counties + 1
)

initial_values <- list(
  beta = rnorm(3, 0, 0.01),  # Assuming 3 fixed effects: year, title_i_eligible, and student_teacher_ratio
  alpha = rep(0, N_schools),
  phi = rep(0, N_counties),
  cov_alpha = diag(1, N_schools),
  cov_phi = diag(1, N_counties),
  tau = 1
)

jags_model <- jags.model(textConnection(model_string), data = jags_data_list, inits = initial_values, n.chains = 3)

results <- coda.samples(model = jags_model, variable.names = c("beta", "alpha", "phi", "cov_alpha", "cov_phi", "sigma2"), n.iter = 10000)



################################
# 3 - Convergence diagnostics
################################

################################
# 4 - Compare models
################################
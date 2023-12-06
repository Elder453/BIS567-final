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
library(RColorBrewer)

################################
# 1 - Pre-modeling preparation
################################

# Load in data
load("data/final_data.RData")

# Set seed
set.seed(567)

# select random schools for graph
n_random = 50
random_schools <- sample(unique(ny$ncessch), n_random)
ny_random <- ny %>% filter (ncessch %in% random_schools)

## PLOT math proficiency over time
palette_colors <- brewer.pal(8, "Set2")
extended_palette <- rep(palette_colors, length.out = n_random)

ggplot(ny_random, aes(x = year, y = math_test_pct_prof_midpt, group = ncessch, color = as.factor(ncessch))) +
  geom_line() +
  scale_color_manual(values = extended_palette) +
  labs(caption = "*Each line represents a unique school",
       x = "Year",
       y = "Math Test Proficiency Midpoint",
       color = "School ID") +
  theme_minimal() +
  scale_x_continuous(breaks = 2011:2017) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  theme(legend.position = "none")

display.brewer.all(type = "qual")  # Show all qualitative palettes



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
ny_jags <- ny[c("year", 
                "logit_math_midpt", 
                "title_i_eligible", 
                "student_teacher_ratio", 
                "school_index", 
                "county_index", 
                "year_index")]

# Standardize continuous predictors
ny_jags$student_teacher_ratio <- scale(ny$student_teacher_ratio)

# Reshape Y to be matrix
Y_matrix <- as.matrix(dcast(ny, school_index ~ year, value.var = "logit_math_midpt")[,-1])

N_schools <- length(unique(ny_jags$school_index))
N_years <- length(unique(ny_jags$year))
N_counties <- length(unique(ny_jags$county_index))

# One-hot encoding for years
for (year in unique(ny_jags$year)) {
  if (year != 2011) {  # Exclude the reference year
    ny_jags[paste("year", year, sep = "_")] <- ifelse(ny_jags$year == year, 1, 0)
  }
}

year_cols <- grep("year_20", names(ny_jags), value = TRUE)
year_cols <- year_cols[year_cols != "year_2011"]  # Exclude the reference year

N_predictors <- 2 + N_years - 1 + 1 # title_i_eligible, student_teacher_ratio, and years, and now Beta0

# Update X_array dimensions
X_array <- array(dim = c(N_schools, N_years, N_predictors))

# Fill the array with new predictors
for (school in unique(ny_jags$school_index)) {
  for (year in unique(ny_jags$year_index)) {
    school_rows <- ny_jags[ny_jags$school_index == school & ny_jags$year_index == year, ]
    if (nrow(school_rows) > 0) {
      X_array[school, year, 1:(N_years - 1)] <- as.numeric(school_rows[year_cols]) # One-hot encoded years
      X_array[school, year, ((N_years-1)+1)] <- school_rows[["title_i_eligible"]]
      X_array[school, year, ((N_years-1)+2)] <- school_rows[["student_teacher_ratio"]]
    }
  }
}
X_array[,,9] <- 1 # intercept, meaning beta[3] is the intercept

################################
# 2 - Formulate model and MCMC
################################

# Read in model strings
model_string_base <- read_file("models/base_lm.txt")
model_string_mixed_1 <- read_file("models/mixed_with_school.txt")
model_string_mixed_2 <- read_file("models/mixed_with_school_and_county.txt")
model_string_mixed_3 <- read_file("models/mixed_with_county.txt")

# Initialize data list and initial values
jags_data_list <- list(
  Y = Y_matrix,
  X = X_array,
  county = ny_jags$county_index,
  N_schools = N_schools,
  N_counties = N_counties,
  N_years = N_years, # include reference year (2011)
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
# model_mixed_3 <- jags.model(textConnection(model_string_mixed_3), 
#                             data = jags_data_list, 
#                             inits = initial_values, 
#                             n.chains = 3)


# Run MCMC and store results
results_base <- coda.samples(model = model_base, 
                             variable.names = c("beta"), 
                             n.iter = 10000)

results_mixed_1 <- coda.samples(model = model_mixed_1, 
                                variable.names = c("beta", "alpha"), 
                                n.iter = 10000)

results_mixed_2 <- coda.samples(model = model_mixed_2, 
                                variable.names = c("beta", "alpha", "phi"), 
                                n.iter = 10000)

# results_mixed_3 <- coda.samples(model = model_mixed_3, 
#                                 variable.names = c("beta", "phi"), 
#                                 n.iter = 10000)

# Save MCMC runs to local disk
save(results_base, file = "data/results_base.RData")
save(results_mixed_1, file = "data/results_mixed_1.RData")
save(results_mixed_2, file = "data/results_mixed_2.RData")
# save(results_mixed_3, file = "data/results_mixed_3.RData")

################################
# 3 - DIC Model Comparisons
################################

# Compute DIC statistics
dic_stats_base <- dic.samples(model = model_base, n.iter = 10000, type = "pD")
dic_stats_mixed_1 <- dic.samples(model = model_mixed_1, n.iter = 10000, type = "pD")
dic_stats_mixed_2 <- dic.samples(model = model_mixed_2, n.iter = 10000, type = "pD")

# > dic_stats_base
# Mean deviance:  17614 
# penalty 10.06
# Penalized deviance: 17624

# > dic_stats_mixed_1
# Mean deviance:  15518 
# penalty 123.2 
# Penalized deviance: 15641

# > dic_stats_mixed_2
# Mean deviance:  15517 
# penalty 122.6
# Penalized deviance: 15640
# Penalized deviance: 15640 
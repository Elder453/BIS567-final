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


################################
# 1 - Pre-modeling preparation
################################

# Load in data
load("data/final_data.RData")

# CREATE indicator matrix for ncessch by county
# Ensure that 'ncessch' and 'county' are factors
ny$ncessch <- as.factor(ny$ncessch)
ny$county <- as.factor(ny$county)

# Get the number of unique 'ncessch' and 'county'
num_ncessch <- length(unique(ny$ncessch))
num_county <- length(unique(ny$county))

# Create a matrix of zeros with the correct dimensions
ncessch_county_matrix <- matrix(0, nrow = num_ncessch, ncol = num_county)

# Name the rows and columns of the matrix
rownames(ncessch_county_matrix) <- sort(unique(ny$ncessch))
colnames(ncessch_county_matrix) <- sort(unique(ny$county))

# Populate the matrix
for (i in 1:nrow(ny)) {
  ncessch_index <- which(rownames(ncessch_county_matrix) == ny$ncessch[i])
  county_index <- which(colnames(ncessch_county_matrix) == ny$county[i])
  ncessch_county_matrix[ncessch_index, county_index] <- 1
}
ncessch_county_matrix

# CREATE indicator matrix for ncessch by observation
num_observations <- nrow(ny)
num_ncessch <- length(levels(ny$ncessch))

# Create a matrix of zeros with the correct dimensions
ncescch_indicator_matrix <- matrix(0, nrow = num_observations, ncol = num_ncessch)

# Name the cols of the matrix after each unique 'ncessch'
colnames(ncescch_indicator_matrix) <- levels(ny$ncessch)

# Populate the matrix
for (i in 1:num_observations) {
  ncessch_value <- ny$ncessch[i]
  column_index <- which(colnames(ncescch_indicator_matrix) == ncessch_value)
  ncescch_indicator_matrix[i, column_index] <- 1
}
ncescch_indicator_matrix 

# PLOT math proficiency over time
ggplot(ny, aes(x = year, y = math_test_pct_prof_midpt, group = ncessch, color = as.factor(ncessch))) +
  geom_line() +
  labs(title = "Math Test Proficiency Midpoint by Year and School",
       x = "Year",
       y = "Math Test Proficiency Midpoint",
       color = "School ID") +
  theme_minimal() + 
  theme(legend.position = "none")


################################
# 2 - Formulate model and MCMC
################################



################################
# 3 - Convergence diagnostics
################################

################################
# 4 - Compare models
################################
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
ny <- load("data/final_data.RData")

# Ensure that 'ncessch' and 'county' are factors
ny$ncessch <- as.factor(ny$ncessch)
ny$county <- as.factor(ny$county)

# Get the number of unique 'ncessch' and 'county'
num_ncessch <- length(unique(ny$ncessch))
num_county <- length(unique(ny$county))

# Create a matrix of zeros with the correct dimensions
matrix_ncessch_county <- matrix(0, nrow = num_ncessch, ncol = num_county)

# Name the rows and columns of the matrix
rownames(matrix_ncessch_county) <- sort(unique(ny$ncessch))
colnames(matrix_ncessch_county) <- sort(unique(ny$county))

# Populate the matrix
for (i in 1:nrow(ny)) {
  ncessch_index <- which(rownames(matrix_ncessch_county) == ny$ncessch[i])
  county_index <- which(colnames(matrix_ncessch_county) == ny$county[i])
  matrix_ncessch_county[ncessch_index, county_index] <- 1
}

# The matrix is now ready
matrix_ncessch_county




# Get the number of observations and the number of unique 'ncessch'
num_observations <- nrow(ny)
num_ncessch <- length(levels(ny$ncessch))

# Create a matrix of zeros with the correct dimensions
indicator_matrix <- matrix(0, nrow = num_observations, ncol = num_ncessch)

# Name the cols of the matrix after each unique 'ncessch'
colnames(indicator_matrix) <- levels(ny$ncessch)

# Populate the matrix
for (i in 1:num_observations) {
  ncessch_value <- ny$ncessch[i]
  column_index <- which(colnames(indicator_matrix) == ncessch_value)
  indicator_matrix[i, column_index] <- 1
}

indicator_matrix


################################
# 2 - Formulate model and MCMC
################################

################################
# 3 - Convergence diagnostics
################################

################################
# 4 - Compare models
################################
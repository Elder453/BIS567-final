#############################################################
## BIS 567 Final Project
## Authors: Eugene Han, Elder Veliz
##
## The purpose of this script is to load the relevant data,
## perform any necessary preprocessing, and save to disk
#############################################################

################################
# 0 - Load libraries
################################
library(educationdata)
library(tidyverse)

################################
# 1 - Query API endpoints
################################
# Note: fips = 8 corresponds to CT
ct_math <- get_education_data(level = "schools", 
                              source = "edfacts",
                              topic = "assessments",
                              filters = list(grade_edfacts = 9,
                                             fips = 9))

# directory

# school finance

################################
# 2 - Build dataset
################################
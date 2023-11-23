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
# Note: 
#   - fips = 9 corresponds to CT
#   - grade_edfacts = 9 corresponds to grades 9-12
#   - school_level = 3 corresponds to High school

# math midpoint proficiency
ct_math <- get_education_data(level = "schools", 
                              source = "edfacts",
                              topic = "assessments",
                              filters = list(grade_edfacts = 9,
                                             fips = 9))
ct_math <- ct_math %>% select(ncessch, year, math_test_pct_prof_midpt)


# title i eligibility
ct_title_ <- get_education_data(level = "schools", 
                                 source = "ccd",
                                 topic = "directory",
                                 filters = list(school_level = 3,
                                                fips = 9))  
ct_title_i <- ct_title_i %>% select(ncessch, year, title_i_eligible)

# number of FT teachers
ct_num_teachers <- get_education_data(level = "schools",
                                       source = "crdc",
                                       topic = "teachers-staff",
                                       filters = list(fips = 9))
ct_num_teachers <- ct_num_teachers %>% select(ncessch, year, teachers_fte_crdc)

# student enrollment
ct_enrollment <- get_education_data(level = "schools",
                                    source = "ccd",
                                    topic = "enrollment",
                                    filters = list(fips = 9,
                                                   year = 2008:2021,
                                                   grade = 9:12))
ct_enrollment <- ct_enrollment %>% select(ncessch, year, enrollment) %>% filter(enrollment > 10)

################################
# 2 - Build dataset
################################
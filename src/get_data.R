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
#   - fips = 36 corresponds to NY
#   - grade_edfacts = 9 corresponds to grades 9-12
#   - school_level = 3 corresponds to High school

FIPS = 36 # New York

# math midpoint proficiency
ct_math <- get_education_data(level = "schools", 
                              source = "edfacts",
                              topic = "assessments",
                              filters = list(grade_edfacts = 9,
                                             fips = FIPS))
ct_math <- ct_math %>% select(ncessch, year, math_test_pct_prof_midpt)

# title i eligibility
ct_title_i <- get_education_data(level = "schools", 
                                 source = "ccd",
                                 topic = "directory",
                                 filters = list(school_level = 3,
                                                fips = FIPS))  
ct_title_i <- ct_title_i %>% select(ncessch, year, title_i_eligible)

# number of FT teachers
ct_num_teachers <- get_education_data(level = "schools",
                                       source = "crdc",
                                       topic = "teachers-staff",
                                       filters = list(fips = FIPS))
ct_num_teachers <- ct_num_teachers %>% 
                    select(ncessch, year, teachers_fte_crdc) %>% 
                    mutate(teachers_fte_crdc = as.integer(teachers_fte_crdc))

# student enrollment
ct_enrollment <- get_education_data(level = "schools",
                                    source = "ccd",
                                    topic = "enrollment",
                                    filters = list(fips = FIPS,
                                                   year = 2008:2021,
                                                   grade = 9:12))
ct_enrollment <- ct_enrollment %>%
                  group_by(year, ncessch) %>%
                  summarize(stud_enrollment = sum(enrollment)) %>% 
                  arrange(ncessch) %>%
                  ungroup()

geo <- get_education_data(level = "schools",
                           source = "nhgis",
                           topic = "census-2010",
                           filters = list(fips = FIPS,
                                          year = 2011:2017))

geo <- geo %>%
        group_by(ncessch) %>%
        slice(1) %>%
        mutate(county = as.factor(county_code)) %>%
        select(ncessch, longitude, latitude, county)

################################
# 2 - Clean & Build dataset
################################

temp <- ct_math %>%
      arrange(ncessch) %>%
      left_join(ct_title_i, by = c("ncessch", "year")) %>%
      left_join(ct_num_teachers, by = c("ncessch", "year")) %>%
      left_join(ct_enrollment, by = c("ncessch", "year")) %>%
      left_join(geo, by = c("ncessch")) %>%
      mutate(ncessch = as.factor(ncessch))

# Impute teachers_fte_crdc
impute_teachers_fte <- function(df, year, school_id) {
  if(year %in% c(2012, 2014, 2016)){
    prev_year = year - 1
    next_year = year + 1
    
    prev_year_value = df$teachers_fte_crdc[df$year == prev_year & df$ncessch == school_id]
    next_year_value = df$teachers_fte_crdc[df$year == next_year & df$ncessch == school_id]
    
    if(length(prev_year_value) == 1 & length(next_year_value) == 1){
      return(mean(c(prev_year_value, next_year_value)))
    } else {
      return(NA)
    }
  } else {
    return(df$teachers_fte_crdc[df$year == year & df$ncessch == school_id])
  }
}

# Impute teachers_fte_crdc
imputed_values <- mapply(impute_teachers_fte, MoreArgs = list(df = temp), 
                         year = temp$year, school_id = temp$ncessch)
temp$teachers_fte_crdc <- imputed_values

temp$stud_enrollment <- as.numeric(as.character(temp$stud_enrollment))
temp$teachers_fte_crdc <- as.numeric(as.character(temp$teachers_fte_crdc))

ny <- temp %>% 
      filter(year %in% 2011:2017,
             title_i_eligible %in% c(0, 1),
             stud_enrollment > 0,
             teachers_fte_crdc > 0,
             math_test_pct_prof_midpt >= 0) %>%
      mutate(student_teacher_ratio = if_else(
                                              !is.na(teachers_fte_crdc) & !is.na(stud_enrollment), # only needed if don't add filter() line
                                              stud_enrollment / teachers_fte_crdc, 
                                              NA_real_)) %>%
      arrange(ncessch)

ggplot(ny, aes(x = year, y = math_test_pct_prof_midpt, group = ncessch, color = as.factor(ncessch))) +
  geom_line() +
  labs(title = "Math Test Proficiency Midpoint by Year and School",
       x = "Year",
       y = "Math Test Proficiency Midpoint",
       color = "School ID") +
  theme_minimal() + 
  theme(legend.position = "none")

# logit transform response 
ny$logit_math_midpt <- log( (ny$math_test_pct_prof_midpt/100) / (1 - ny$math_test_pct_prof_midpt/100) )
hist(ny$logit_math_midpt)

# 2013: NY state adopts tougher benchmarks for proficiency (adjusting to Common Core) --> 2013 has huge declines in relate to 2012 but recovers 2014
  # indicator variable for 2013 (common core adoption)

# Save data as CSV
save(ny, file = "final_data.rdata")





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







# ST.CARanova()
# ranodm effect that is time (county-specific), learns about space and time separately (can turn off interaction term) 
# replication --> multi-level version: multivariate: k = county, t= time, j = school;
# MVST.CARar()
# AR1 at each time point there's specific set of effects (e.g. counties); vectors subjected to auto reg structure; in anova there's one static set of spatial and temporal paramteters. 
# hard to learn correlation over time; won't be able to learn about rho t (pg 15); year effect
# ignore time --> dummy for vars

# JAGS, can't do spatial part but can get hierarchical structure

# shape file for counties to use carBayes




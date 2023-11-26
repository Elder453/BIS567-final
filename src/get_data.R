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

################################
# 2 - Clean & Build dataset
################################

temp <- ct_math %>%
      arrange(ncessch) %>%
      left_join(ct_title_i, by = c("ncessch", "year")) %>%
      left_join(ct_num_teachers, by = c("ncessch", "year")) %>%
      left_join(ct_enrollment, by = c("ncessch", "year"))

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
                                              NA_real_))

ggplot(ny, aes(x = year, y = math_test_pct_prof_midpt, group = ncessch, color = as.factor(ncessch))) +
  geom_line() +
  labs(title = "Math Test Proficiency Midpoint by Year and School",
       x = "Year",
       y = "Math Test Proficiency Midpoint",
       color = "School ID") +
  theme_minimal() + 
  theme(legend.position = "none")

# distribution/outlier threshold --> need to log transform response (midpt math %)
hist(ny$math_test_pct_prof_midpt)
mean(ny$math_test_pct_prof_midpt) - 3*sd(ny$math_test_pct_prof_midpt)

# dist/potential outliers (student enrollment)
hist(ny$stud_enrollment)
mean(ny$stud_enrollment) + 3*sd(ny$stud_enrollment)
# 2013: NY state adopts tougher benchmarks for proficiency (adjusting to Common Core) --> 2013 has huge declines in relate to 2012 but recovers 2014
  # indicator variable for 2013 (common core adoption)

# Save data as CSV
#write.csv(ny, file = "final_data.csv")



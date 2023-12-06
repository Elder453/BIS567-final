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

# Notes: 
#   - fips = 36 corresponds to NY
#   - grade_edfacts = 9 corresponds to grades 9-12
#   - school_level = 3 corresponds to High school

FIPS = 36 # New York

# math midpoint proficiency
ny_math <- get_education_data(level = "schools", 
                              source = "edfacts",
                              topic = "assessments",
                              filters = list(grade_edfacts = 9,
                                             fips = FIPS,
                                             year = 2011:2017))
ny_math <- ny_math %>% select(ncessch, year, math_test_pct_prof_midpt)

# title i eligibility
ny_title_i <- get_education_data(level = "schools", 
                                 source = "ccd",
                                 topic = "directory",
                                 filters = list(school_level = 3,
                                                fips = FIPS,
                                                year = 2011:2017))  
ny_title_i <- ny_title_i %>% select(ncessch, year, title_i_eligible)

# number of FT teachers
ny_num_teachers <- get_education_data(level = "schools",
                                       source = "crdc",
                                       topic = "teachers-staff",
                                       filters = list(fips = FIPS,
                                                      year = seq(2011,2017, 2)))
ny_num_teachers <- ny_num_teachers %>% 
                    select(ncessch, year, teachers_fte_crdc) %>% 
                    mutate(teachers_fte_crdc = as.integer(teachers_fte_crdc))

# student enrollment
ny_enrollment <- get_education_data(level = "schools",
                                    source = "ccd",
                                    topic = "enrollment",
                                    filters = list(fips = FIPS,
                                                   year = 2011:2017,
                                                   grade = 9:12))
ny_enrollment <- ny_enrollment %>%
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
# 2 - Clean & build dataset
################################

temp <- ny_math %>%
      arrange(ncessch) %>%
      left_join(ny_title_i, by = c("ncessch", "year")) %>%
      left_join(ny_num_teachers, by = c("ncessch", "year")) %>%
      left_join(ny_enrollment, by = c("ncessch", "year")) %>%
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

ny <- ny %>%
      group_by(ncessch) %>%
      filter(all(2011:2017 %in% year)) %>%
      ungroup()

# logit transform response 
ny$logit_math_midpt <- log( (ny$math_test_pct_prof_midpt/100) / (1 - ny$math_test_pct_prof_midpt/100) )

# 2013: NY state adopts tougher benchmarks for proficiency (adjusting to Common Core) --> 2013 has huge declines in relate to 2012 but recovers 2014
  # indicator variable for 2013 (common core adoption)

################################
# 3 - Save final dataset
################################

save(ny, file = "data/final_data.RData")

######################################
# Miscellaneous notes - DELETE LATER
######################################

# ST.CARanova()
# ranodm effect that is time (county-specific), learns about space and time separately (can turn off interaction term) 
# replication --> multi-level version: multivariate: k = county, t= time, j = school;
# MVST.CARar()
# AR1 at each time point there's specific set of effects (e.g. counties); vectors subjected to auto reg structure; 
# in anova there's one static set of spatial and temporal paramteters. 
# hard to learn correlation over time; won't be able to learn about rho t (pg 15); year effect
# ignore time --> dummy for vars

# JAGS, can't do spatial part but can get hierarchical structure

# shape file for counties to use carBayes

# Stuyvesant: https://nces.ed.gov/ccd/schoolsearch/school_detail.asp?Search=1&InstName=Stuyvesant&SchoolType=1&SchoolType=2&SchoolType=3&SchoolType=4&SpecificSchlTypes=all&IncGrade=-1&LoGrade=-1&HiGrade=-1&ID=360007702877
ny %>% filter(ncessch == 360007702877)

# Amsterdam: https://nces.ed.gov/ccd/schoolsearch/school_detail.asp?Search=1&InstName=Amsterdam&State=36&SchoolType=1&SchoolType=2&SchoolType=3&SchoolType=4&SpecificSchlTypes=all&IncGrade=-1&LoGrade=-1&HiGrade=-1&ID=360297000068
ny %>% filter(ncessch == 360297000068)

# QUEENS SATELLITE HIGH SCHOOL FOR OPPORTUNITY : https://nces.ed.gov/ccd/schoolsearch/school_detail.asp?Search=1&SchoolID=360010006182&State=36&SchoolType=1&SchoolType=2&SchoolType=3&SchoolType=4&SpecificSchlTypes=all&IncGrade=-1&LoGrade=-1&HiGrade=-1&ID=360010006182
ny %>% filter(ncessch == 360010006182)

ny %>% filter(ncessch == 360009101928)


temp1 <- get_education_data(level = "schools", 
                                 source = "ccd",
                                 topic = "directory",
                                 filters = list(school_level = 3,
                                                fips = FIPS,
                                                year = 2011:2017))  
temp1 <- temp1 %>% select(ncessch, school_name)

school_indices <- c(360000800193, 
                    360002101704, 
                    360007600624, 
                    360007603352, 
                    360007603680, 
                    360007605621, 
                    360007700116,
                    360007700585,
                    360007700595,
                    360007700637,
                    360007700649,
                    360007700691,
                    360007700692,
                    360007702871,
                    360007705084,
                    360007705085,
                    360007705622,
                    360007705624,
                    360007705625,
                    360007705764,
                    360007705770,
                    360007705771,
                    360007805113)

tmp <- temp1 %>% filter(ncessch %in% school_indices) %>% unique()

ny %>% filter(ncessch == 360007605621)

(tmp) %>% group_by(ncessch) %>% summarize(mean = mean(math_test_pct_prof_midpt),
                                          city_mailing) %>% arrange(mean) %>% filter(city_mailing != "NEW YORK", city_mailing != "BRONX", city_mailing != "BROOKLYN") %>% unique() %>% print(n=100)
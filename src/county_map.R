#############################################################
## BIS 567 Final Project
## Authors: Eugene Han, Elder Veliz
##
## The purpose of this script is to visualize the results
## of the county random effects using a NY heatmap
#############################################################

################################
# 0 - Load libraries
################################

# Load the sf package
library(sf)
library(ggplot2)
library(sf)
library(viridis)

################################
# 1 - Preparing the data
################################

all_states <- st_read("data/tl_2023_us_county.shp")

# filter for NY counties
ny_data <- all_states[all_states$STATEFP == 36, ]

ny_data[ny_data$NAMELSAD == "Hamilton County",]

# join phi values and county codes
index = unique(as.numeric(as.factor(ny$county)))
county_code = as.character(unique(as.factor(ny$county)))
county_data <- as.data.frame(index, county_code)
county_data$county_code = county_code
county_data$phi_mean = rep(NA, 61)
for (i in 1:61) {
  county_data$phi_mean[i] = summary(results_mixed_2[, paste0("phi[", index[i], "]"), drop = FALSE])$statistics[1]
}

sum(county_data$county_code %in% ny_data$GEOID)
merged_data <- merge(ny_data, county_data[-1], by.x = "GEOID", by.y = "county_code")
merged_data$NAME
################################
# 2 - Creating the heatmap
################################

# Plot the heatmap
ggplot(data = merged_data) +
  geom_sf(aes(fill = phi_mean)) +  # fill counties based on average_value
  scale_fill_viridis(option = "A", direction = -1) +  # color scale
  labs(fill = "Mean φ Value",
       main = "New York Counties") +
  theme_minimal()  # theme

hist(merged_data$phi_mean)
median(merged_data$phi_mean)

summary(results_mixed_2[, "beta[8]", drop = FALSE])$statistics[1]

ny_jags %>% summarize(meanst = mean(student_teacher_ratio))
ny_jags %>% filter(county_index == 18) %>% summarize(meanst = mean(logit_math_midpt))
ny_jags %>% filter(county_index == 23) %>% summarize(mean_logit_math = mean(logit_math_midpt))


compare = ny_jags %>% group_by(county_index) %>% summarize(mean_logit_math = mean(logit_math_midpt)) %>% merge(county_data, by.x = "county_index", by.y = "index")
plot(compare$phi_mean ~ compare$mean_logit_math)

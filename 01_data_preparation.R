

rm(list = ls()) # clearing the history from previous analyses

getwd()
file.exists("C:/Users/Zahida/.git")
system("git rev-parse --show-toplevel")

library(tidyverse) # dplyr for data manipulation, tidyr for reshaping data, readr for importing data


### Natural England Project ###

##### Field Survey Dataset #####

data_field <- read_csv("C:/Users/zahid/Dropbox/Job Applications/Natural England/Data Analysis/Year 1 EES Soil SA Field Survey Dataset.csv")
data_lab <- read_csv("C:/Users/zahid/Dropbox/Job Applications/Natural England/Data Analysis/Year 1 EES Soil SA Lab Biological Dataset CO2 Rate.csv")

glimpse(data_field)
glimpse(data_lab)

str(data_field)

names(data_field)
names(data_lab)

n_distinct(data_field$plot_number)
unique(data_field$plot_number)
n_distinct(data_field$sampling_point)
unique(data_field$sampling_point)



field_small <- data_field %>%
  select(
    delocated_monad,
    plot_number,
    sampling_point,
    easting_centroid_delocated,
    northing_centroid_delocated,
    earthworm_num_5min,
    earthworm_num_5min_qualifier,
    earthworm_additional,
    earthworm_additional_qualifier,
    soil_temperature,
    soil_temperature_qualifier
  )

lab_small <- data_lab %>%
  select(
    delocated_monad,
    plot_number,
    soil_moisture_content,
    soil_moisture_content_qualifier,
    WAT_CO2,
    WAT_CO2_qualifier
  )

soil_data <- field_small %>%
  inner_join(
    lab_small,
    by = c("delocated_monad", "plot_number")
  )


glimpse(soil_data)
summary(soil_data)
dim(soil_data)
names(soil_data)

hist(soil_data$earthworm_num_5min, 60)

nrow(field_small) # check how many rows the main field data had
nrow(soil_data) # check how many rows the new joined data has

ggplot(soil_data, aes(x = earthworm_num_5min)) +
  geom_histogram(binwidth = 1) +
  labs(
    x = "Number of earthworms (first 5 min)",
    y = "Frequency",
    title = "Distribution of earthworm counts"
  ) +
  theme_minimal()

ggplot(soil_data, aes(x = WAT_CO2, y = earthworm_num_5min)) +
  geom_jitter(width = 0.1, height = 0.15, alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "X",
    y = "Number of earthworms",
    title = "Earthworm abundance vs X"
  ) +
  theme_minimal()

table(soil_data$earthworm_num_5min)
mean(soil_data$earthworm_num_5min == 0, na.rm = TRUE)



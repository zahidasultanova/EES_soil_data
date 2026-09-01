


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

library(lme4)

model_pois <- glmer(
  earthworm_num_5min ~ soil_temperature +          # Effect of soil temperature
    soil_moisture_content +                        # Effect of soil moisture
    WAT_CO2 +                                      # Effect of microbial respiration
    (1 | delocated_monad/plot_number),             # Control for monad and plots within monads
  family = poisson(link = "log"),                  # Poisson distribution for count data
  data = soil_data,
  na.action = na.omit                              # Exclude rows missing variables used in the model
)

R.version.string
Sys.which("make")
file.exists("C:/rtools45")
file.exists("C:/rtools44")
file.exists("C:/rtools43")

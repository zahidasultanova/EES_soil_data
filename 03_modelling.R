
library(lme4)
library(performance)
library(DHARMa)
library(glmmTMB)
library(mgcv)
library(splines2)

soil_data <- read_csv("C:/Users/zahid/Dropbox/Job Applications/Natural England/Data Analysis/data/soil_data.csv")
soil_data_clean <- soil_data[complete.cases(soil_data[, vars_needed]), ]

soil_data_clean$delocated_monad <- as.factor(soil_data_clean$delocated_monad)
soil_data_clean$plot_number <- as.factor(soil_data_clean$plot_number)

cor(soil_data[, c("soil_temperature", "soil_moisture_content", "WAT_CO2")], 
    use = "complete.obs")

vars_needed <- c("earthworm_num_5min", "soil_temperature", 
                 "soil_moisture_content", "WAT_CO2", 
                 "delocated_monad", "plot_number")

nrow(soil_data)         # original
nrow(soil_data_clean)   # after dropping incomplete rows — note how many you lost

## Strong positive correlation between "soil_moisture_content" and "WAT_CO2". I keep soil moisture for the model.

hist(soil_data$soil_temperature)
hist(soil_data$soil_moisture_content)
hist(soil_data$WAT_CO2)
boxplot(soil_data$soil_temperature)

table(soil_data$earthworm_num_5min == 0)
mean(soil_data$earthworm_num_5min == 0)


model_pois <- glmer(
  earthworm_num_5min ~ soil_temperature +          # Effect of soil temperature
    soil_moisture_content +                        # Effect of soil moisture
    (1 | delocated_monad/plot_number),             # Control for monad and plots within monads
  family = poisson(link = "log"),                  # Poisson distribution for count data
  data = soil_data_clean)

summary(model_pois)

check_overdispersion(model_pois)

model_nb2 <- glmmTMB(
  earthworm_num_5min ~ soil_temperature +
    soil_moisture_content +
    (1 | delocated_monad/plot_number),
  family = nbinom2(link = "log"),   # variance = mu + mu^2/theta
  data = soil_data_clean)

model_nb1 <- glmmTMB(
  earthworm_num_5min ~ soil_temperature +
    soil_moisture_content +
    (1 | delocated_monad/plot_number),
  family = nbinom1(link = "log"),   # variance = mu + mu^2/theta
  data = soil_data_clean)

check_overdispersion(model_nb1)
check_overdispersion(model_nb2)

AIC(model_pois, model_nb2, model_nb1)

summary(model_nb1)

check_zeroinflation(model_nb1)

sim_res <- simulateResiduals(model_nb1)
plot(sim_res)                        # QQ plot + residual vs predicted, with tests
testDispersion(sim_res)              # no overdispersion
testZeroInflation(sim_res)           # zero-inflation, cross-check
testOutliers(sim_res)                # outlier detection
testUniformity(sim_res)              # are residuals uniformly distributed (the DHARMa analog of normality)


model_zinb1 <- glmmTMB(
  earthworm_num_5min ~ soil_temperature +
    soil_moisture_content +
    (1 | delocated_monad/plot_number),
  ziformula = ~1,                     # simplest: constant zero-inflation probability
  family = nbinom1(link = "log"),
  data = soil_data_clean)

model_zinb2 <- glmmTMB(
  earthworm_num_5min ~ soil_temperature +
    soil_moisture_content +
    (1 | delocated_monad/plot_number),
  ziformula = ~ soil_moisture_content + soil_temperature,   # zero-part predictors
  family = nbinom1(link = "log"),
  data = soil_data_clean)

soil_data_clean

AIC(model_nb1, model_zinb1, model_zinb2)

summary(model_zinb1)

sim_zinb1 <- simulateResiduals(model_zinb1, n = 1000)

plot(sim_zinb1)

testDispersion(sim_zinb1)
testZeroInflation(sim_zinb1)
testUniformity(sim_zinb1)
testQuantiles(sim_zinb1)


colSums(is.na(soil_data_clean))

nrow(soil_data_clean)
nrow(model_zinb2$frame)
nrow(soil_data_clean) == nrow(model_zinb2$frame)

model_zinb2 <- glmmTMB(
  earthworm_num_5min ~ soil_temperature +
    soil_moisture_content +
    WAT_CO2 +
    (1 | delocated_monad/plot_number),
  ziformula = ~ soil_moisture_content + soil_temperature,
  family = nbinom1(link = "log"),
  data = soil_data_clean          # make sure this says soil_data_clean, not soil_data
)

sim_zinb2 <- simulateResiduals(model_zinb2, n = 1000)


plotResiduals(sim_zinb1, soil_data_clean$soil_temperature, quantreg = TRUE)
plotResiduals(sim_zinb1, soil_data_clean$soil_moisture_content, quantreg = TRUE)
plotResiduals(sim_zinb1, soil_data_clean$WAT_CO2, quantreg = TRUE)


model_gam_check <- gam(
  earthworm_num_5min ~ s(soil_temperature) +
    s(soil_moisture_content) +
    s(delocated_monad, bs = "re") +
    s(plot_number, bs = "re"),
  family = nb(link = "log"),
  data = soil_data_clean
)

summary(model_gam_check)   # look at edf per smooth term
plot(model_gam_check, pages = 1, residuals = TRUE, shade = TRUE)

model_zinb_spline <- glmmTMB(
  earthworm_num_5min ~ ns(soil_temperature, df = 4) +
    ns(soil_moisture_content, df = 6) +
    (1 | delocated_monad/plot_number),
  ziformula = ~ soil_moisture_content + soil_temperature,
  family = nbinom1(link = "log"),
  data = soil_data_clean)

summary(model_zinb_spline)
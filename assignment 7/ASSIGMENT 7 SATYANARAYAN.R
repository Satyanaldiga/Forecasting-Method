
library(fpp3)

# Load the dataset
data("aus_arrivals")

# Create the training set by withholding the last two years (8 quarters) of data
train_data <- aus_arrivals %>%
  filter(Quarter < yearquarter("2010 Q4"))

test_data <- aus_arrivals %>%
  filter(Quarter >= yearquarter("2010 Q4"))

# Plot the time series data
autoplot(aus_arrivals, Arrivals) +
  labs(title = "Quarterly Arrivals from New Zealand to Australia", y = "Arrivals", x = "Quarter")

# Seasonal and subseries plots
gg_season(aus_arrivals, Arrivals) +
  labs(title = "Seasonal Plot: Arrivals to Australia from New Zealand")

gg_subseries(aus_arrivals, Arrivals) +
  labs(title = "Subseries Plot: Arrivals to Australia from New Zealand")

# ARIMA Model (pdq(1,1,1) + PDQ(1,1,1))
arima_model <- train_data %>%
  model(ARIMA(Arrivals ~ pdq(1,1,1) + PDQ(1,1,1)))

# Automatic ARIMA (for comparison)
auto_arima <- train_data %>%
  model(auto = ARIMA(Arrivals))

# ETS model (for comparison)
ets_model <- train_data %>%
  model(ETS(Arrivals))

# TSLM model (for comparison)
tslm_model <- train_data %>%
  model(TSLM(Arrivals ~ trend() + season()))

# Seasonal Naive model (for comparison)
snaive_model <- train_data %>%
  model(SNAIVE(Arrivals))

# Forecast for each model
fc_arima <- arima_model %>% forecast(h = "2 years")
fc_auto_arima <- auto_arima %>% forecast(h = "2 years")
fc_ets <- ets_model %>% forecast(h = "2 years")
fc_tslm <- tslm_model %>% forecast(h = "2 years")
fc_snaive <- snaive_model %>% forecast(h = "2 years")

# Combine all forecasts for comparison
combined_fc <- bind_rows(
  fc_arima %>% mutate(Model = "ARIMA(1,1,1)(1,1,1)"),
  fc_auto_arima %>% mutate(Model = "Auto ARIMA"),
  fc_ets %>% mutate(Model = "ETS"),
  fc_tslm %>% mutate(Model = "TSLM"),
  fc_snaive %>% mutate(Model = "SNaive")
)

# Plot the forecasts
combined_fc %>%
  autoplot(aus_arrivals, level = NULL) +
  labs(title = "Forecast Comparison", y = "Arrivals", x = "Quarter") +
  facet_wrap(~ Model)

# Accuracy comparison
accuracy_table <- accuracy(combined_fc, test_data)

# Display the accuracy table to compare which model performed best
accuracy_table

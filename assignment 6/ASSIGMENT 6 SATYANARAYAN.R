# Load necessary libraries
library(fpp3)

# Load the aus_arrivals dataset (part of fpp3 package)
data("aus_arrivals")

# Check the structure of the dataset to verify columns and format
str(aus_arrivals)

# Filter the dataset for only New Zealand arrivals (use "NZ" instead of "New Zealand")
aus_arrivals_nz <- aus_arrivals %>%
  filter(Origin == "NZ")

# Convert 'Quarter' to a time index if necessary
aus_arrivals_nz <- aus_arrivals_nz %>%
  as_tsibble(index = Quarter)

# Split the data into training and test sets (withholding the last 2 years)
train_data <- aus_arrivals_nz %>%
  filter_index("1981 Q1" ~ "2010 Q3")  # Up to 2010 Q3 for training
test_data <- aus_arrivals_nz %>%
  filter_index("2010 Q4" ~ "2012 Q3")  # Last two years as test set

# 1. Visualize the data

# Autoplot of the data
autoplot(train_data, Arrivals) + 
  ggtitle("Quarterly Number of Arrivals to Australia from New Zealand")

# Seasonal plot
gg_season(train_data, Arrivals) +
  ggtitle("Seasonal Plot of Arrivals to Australia from New Zealand")

# Subseries plot
gg_subseries(train_data, Arrivals) +
  ggtitle("Subseries Plot of Arrivals to Australia from New Zealand")

# 2. Fit ETS model

# Fit ETS model to the training data
fit_ets <- train_data %>%
  model(ETS(Arrivals))

# Summary of the ETS model
report(fit_ets)

# 3. Fit a time series regression with trend and season

# Fit time series regression with trend and seasonality
fit_reg <- train_data %>%
  model(TSLM(Arrivals ~ trend() + season()))

# Summary of the time series regression model
report(fit_reg)

# 4. Fit a seasonal naive model

# Seasonal naive model (uses seasonality from the last observation)
fit_snaive <- train_data %>%
  model(SNAIVE(Arrivals ~ season()))

# Summary of the seasonal naive model
report(fit_snaive)

# 5. Forecasting

# Forecast using ETS model for the next 2 years (8 quarters)
ets_forecast <- fit_ets %>%
  forecast(h = "2 years")

# Forecast using time series regression model for the next 2 years
reg_forecast <- fit_reg %>%
  forecast(h = "2 years")

# Forecast using seasonal naive model for the next 2 years
snaive_forecast <- fit_snaive %>%
  forecast(h = "2 years")

# 6. Plot the forecasts and compare with the test set

# Plot ETS forecast vs test data
autoplot(ets_forecast, test_data) +
  ggtitle("ETS Forecast vs Test Set")

# Plot time series regression forecast vs test data
autoplot(reg_forecast, test_data) +
  ggtitle("Regression Forecast vs Test Set")

# Plot seasonal naive forecast vs test data
autoplot(snaive_forecast, test_data) +
  ggtitle("Seasonal Naive Forecast vs Test Set")

# 7. Evaluate the accuracy of the forecasts

# Calculate accuracy of each model's forecast against the test data
ets_accuracy <- accuracy(ets_forecast, test_data)
reg_accuracy <- accuracy(reg_forecast, test_data)
snaive_accuracy <- accuracy(snaive_forecast, test_data)

# Display accuracy results
ets_accuracy
reg_accuracy
snaive_accuracy

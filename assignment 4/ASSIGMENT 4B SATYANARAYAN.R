# Load the required library
library(fpp3)

# Load the aus_retail dataset
data("aus_retail")

# Convert Month column to yearmonth format and aggregate across all states for "Takeaway food services"
takeaway_data <- aus_retail %>%
  filter(Industry == "Takeaway food services") %>%
  mutate(Month = yearmonth(Month)) %>%  
  index_by(Month) %>%
  summarise(Turnover = sum(Turnover))

# Check the structure of the takeaway_data time series
glimpse(takeaway_data)

# Plot the takeaway food turnover data
autoplot(takeaway_data) +
  ggtitle("Australian Takeaway Food Turnover") +
  ylab("Turnover (Million AUD)") +
  xlab("Year")

# Create the training set by withholding the last four years (before Jan 2017)
takeaway_train <- takeaway_data %>%
  filter(Month < yearmonth("2017 Jan"))

# Create the test set (from Jan 2017 onward)
takeaway_test <- takeaway_data %>%
  filter(Month >= yearmonth("2017 Jan"))

# Plot the training and test sets together
autoplot(takeaway_data) +
  autolayer(takeaway_train, series = "Training Set", color = "green") +
  autolayer(takeaway_test, series = "Test Set", color = "orange") +
  ggtitle("Training and Test Sets for Takeaway Food Turnover") +
  ylab("Turnover (Million AUD)") +
  xlab("Year") +
  theme(legend.position = "bottom")

# Fit benchmark forecasting methods on the training set
fit <- takeaway_train %>%
  model(
    Mean = MEAN(Turnover),
    Naive = NAIVE(Turnover),
    SNaive = SNAIVE(Turnover),
    Drift = RW(Turnover ~ drift())
  )

# Forecast using the fitted models for the test period
fc <- fit %>%
  forecast(h = nrow(takeaway_test))

# Plot the forecasts along with the actual turnover data
autoplot(takeaway_data) +
  autolayer(fc, series = "Forecasts", color = "blue") +
  ggtitle("Benchmark Forecasts for Takeaway Food Turnover") +
  ylab("Turnover (Million AUD)") +
  xlab("Year") +
  theme(legend.position = "bottom")

# Compute forecast accuracy for each model using the test set
accuracy_fc <- fc %>%
  accuracy(takeaway_test)

# View the accuracy results
print(accuracy_fc)

# Identify the best-performing model based on RMSE
best_model <- accuracy_fc %>%
  arrange(RMSE) %>%
  slice(1) %>%
  pull(.model)

# Print the name of the best model based on RMSE
print(paste("The best model based on RMSE is:", best_model))


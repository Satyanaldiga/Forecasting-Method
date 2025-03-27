# Load necessary libraries
library(tsibble)
library(dplyr)
library(ggplot2)
library(fable)
library(fabletools)


# Load the Amtrak dataset (replace with the path to your file)
Amtrak.df <- read.csv("C:\\Users\\SPURGE\\Desktop\\forecast\\Amtrak.csv")

# Convert the data to a tsibble
Amtrak.df <- Amtrak.df %>%
  mutate(Date = as.Date(Month, "%d/%m/%Y")) %>%
  mutate(Month = yearmonth(Date))

ridership.tb <- Amtrak.df %>%
  as_tsibble(index = Month)

# Fill in missing values (if necessary)
ridership.tb <- tsibble::fill_gaps(ridership.tb)

# Split the data into training and validation sets
train_data <- filter(ridership.tb, Month <= yearmonth("1999 Dec"))
test_data <- filter(ridership.tb, Month > yearmonth("1999 Dec"))

# Plot the data to explore trends and seasonality
library(feasts)
autoplot(ridership.tb, Ridership) + ggtitle("Amtrak Ridership Over Time")
gg_season(ridership.tb, Ridership) + ggtitle("Seasonal Plot of Amtrak Ridership")
gg_subseries(ridership.tb, Ridership) + ggtitle("Subseries Plot of Amtrak Ridership")

# Fit a time series regression model with trend and season
model1 <- model(train_data, TSLM(Ridership ~ trend() + season()))

# Forecast using the first model
forecast1 <- forecast(model1, new_data = test_data)

# Plot the forecast
autoplot(forecast1) + ggtitle("Forecast using Linear Trend + Season Model")

# Assess the accuracy of the model
accuracy1 <- accuracy(forecast1, test_data)

# Fit a time series regression model with a quadratic trend and regular season
model2 <- model(train_data, TSLM(Ridership ~ trend() + I(trend()^2) + season()))

# Forecast using the second model
forecast2 <- forecast(model2, new_data = test_data)

# Plot the forecast
autoplot(forecast2) + ggtitle("Forecast using Quadratic Trend + Season Model")

# Assess the accuracy of the second model
accuracy2 <- accuracy(forecast2, test_data)

# Print accuracy results
print("Accuracy of Linear Trend + Season Model:")
print(accuracy1)
print("Accuracy of Quadratic Trend + Season Model:")
print(accuracy2)

# Compare the accuracy of both models to determine which is more accurate
if (accuracy2$RMSE < accuracy1$RMSE) {
  print("The Quadratic Trend + Season model is more accurate.")
} else {
  print("The Linear Trend + Season model is more accurate.")
}


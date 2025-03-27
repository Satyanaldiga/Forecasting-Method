# Load necessary libraries
library(fpp3)
library(seasonal)  # For SEATS/X-11 decomposition

# Load the canadian_gas dataset
data("canadian_gas")

# Plot the data using autoplot
autoplot(canadian_gas) +
  labs(title = "Monthly Canadian Gas Production",
       y = "Billions of cubic metres",
       x = "Year")

# Plot using gg_subseries to observe seasonal patterns
canadian_gas %>%
  gg_subseries(Volume) +
  labs(y = "Billions of cubic metres",
       title = "Subseries Plot: Canadian Gas Production")

# Plot using gg_season to observe seasonal effects
canadian_gas %>%
  gg_season(Volume, labels = "both") +
  labs(y = "Billions of cubic metres",
       title = "Seasonal Plot: Canadian Gas Production")

# STL decomposition (choose a seasonal window based on your analysis)
stl_decomp <- canadian_gas %>%
  model(STL(Volume ~ season(window = "periodic")))

components(stl_decomp) %>%
  autoplot() +
  labs(title = "STL Decomposition of Canadian Gas Production")

# Observing how the seasonal shape changes over time
stl_decomp %>%
  components() %>%
  gg_season(season_year, labels = "both") +
  labs(title = "Seasonal Shape Over Time")

# SEATS decomposition using X_13ARIMA_SEATS()
seats_decomp <- canadian_gas %>%
  model(seats = X_13ARIMA_SEATS(Volume ~ seats()))

# Extract components and plot SEATS decomposition
components(seats_decomp) %>%
  autoplot() +
  labs(title = "SEATS Decomposition of Canadian Gas Production")

# X-11 decomposition
x11_decomp <- canadian_gas %>%
  model(X_13ARIMA_SEATS(Volume ~ x11()))

components(x11_decomp) %>%
  autoplot() +
  labs(title = "X-11 Decomposition of Canadian Gas Production")


# Extracting components for comparison
stl_components <- components(stl_decomp) %>%
  mutate(type = "STL")

seats_components <- components(seats_decomp) %>%
  mutate(type = "SEATS")

x11_components <- components(x11_decomp) %>%
  mutate(type = "X-11")

# Combine all components into one dataset for comparison
combined_components <- bind_rows(stl_components, seats_components, x11_components)

# Plot comparison of seasonal components
combined_components %>%
  ggplot(aes(x = Month, y = season_adjust, color = type)) +
  geom_line() +
  facet_wrap(~ type, scales = "free_y") +
  labs(title = "Comparison of Seasonal Components: STL vs SEATS vs X-11",
       y = "Seasonal Component",
       x = "Year")

# Plot comparison of trend components
combined_components %>%
  ggplot(aes(x = Month, y = trend, color = type)) +
  geom_line() +
  facet_wrap(~ type, scales = "free_y") +
  labs(title = "Comparison of Trend Components: STL vs SEATS vs X-11",
       y = "Trend Component",
       x = "Year")

# Plot comparison of remainder components (residuals)
combined_components %>%
  ggplot(aes(x = Month, y = remainder, color = type)) +
  geom_line() +
  facet_wrap(~ type, scales = "free_y") +
  labs(title = "Comparison of Remainder Components: STL vs SEATS vs X-11",
       y = "Remainder Component",
       x = "Year")







# Compare seasonal patterns across methods
#cbind(
#  STL = components(stl_decomp)$season,
#  SEATS = components(seats_decomp)$season,
#  X11 = components(x11_decomp)$season
#) %>%
#  autoplot() +
#  labs(title = "Comparison of Seasonal Patterns: STL vs SEATS vs X-11",
#       y = "Seasonal Component")


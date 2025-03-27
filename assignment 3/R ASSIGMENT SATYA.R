# Install the packages if not already installed
install.packages("fpp3") # contains forecast and plotting functions
install.packages("ggplot2")

# Load the libraries
library(fpp3)
library(ggplot2)


# Load datasets
data("aus_production")
data("pelt")
data("us_gasoline")

# Inspect datasets
help("aus_production")
help("pelt")
help("us_gasoline")

# Bricks from aus_production
bricks <- aus_production %>% filter(!is.na(Bricks))
print(bricks %>% pull(Quarter)) # To get the time index of the Bricks data

# Hare from pelt
hare <- pelt %>% filter(!is.na(Hare))
print(hare %>% pull(Year)) # To get the time index of the Hare data

# Barrels from us_gasoline
barrels <- us_gasoline %>% filter(!is.na(Barrels))
print(barrels %>% pull(Week)) # To get the time index of the Barrels data

# Bricks from aus_production
autoplot(bricks, Bricks) + ggtitle("Bricks Production")
gg_season(bricks, Bricks) + ggtitle("Seasonality of Bricks Production")
gg_subseries(bricks, Bricks) + ggtitle("Subseries Plot of Bricks Production")

# Hare from pelt
autoplot(hare, Hare) + ggtitle("Hare Pelts")
gg_subseries(hare, Hare) + ggtitle("Subseries Plot of Hare Pelts")

# Barrels from us_gasoline
autoplot(barrels, Barrels) + ggtitle("Gasoline Barrels")
gg_season(barrels, Barrels) + ggtitle("Seasonality of Gasoline Barrels")
gg_subseries(barrels, Barrels) + ggtitle("Subseries Plot of Gasoline Barrels")

library(dplyr)
# Grouping Barrels data by year and calculating average production
barrels_summary <- us_gasoline %>%
  mutate(Year = year(Week)) %>%  # Extract Year from the Week index
  group_by(Year) %>%
  summarize(avg_barrels = mean(Barrels, na.rm = TRUE))

barrels_summary

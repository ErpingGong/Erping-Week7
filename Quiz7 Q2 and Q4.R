library(tibble)
library(tidyverse)
library(rstanarm)
library(ggplot2)
set.seed(42) # For reproducibility

buildings<- tibble(
  BuildingID = 1:100,
  NumFloors = sample(1:60, 100, replace = TRUE), # Random number of floors between 1 and 60
  YearConstructed = sample(1900:2020, 100, replace = TRUE), # Random construction year between 1900 and 2020
  BuildingUse = sample(c("Residential", "Commercial", "Mixed"), 100, replace = TRUE), # Building use
  LocationType = sample(c("Central", "Peripheral"), 100, replace = TRUE) # Location type
)
head(data)

ggplot(buildings, aes(x = YearConstructed, y = NumFloors, color = BuildingUse)) +
  geom_point() +
  facet_wrap(~LocationType) +
  theme_minimal() +
  labs(title = "Number of Floors by Year of Construction, Use, and Location",
       x = "Year of Construction",
       y = "Number of Floors")


buildings$BuildingUse <- as.factor(buildings$BuildingUse)
buildings$LocationType <- as.factor(buildings$LocationType)
model_enhanced <- stan_glm(NumFloors ~ YearConstructed + BuildingUse + LocationType,
                           data = buildings, family = gaussian())
print(summary(model_enhanced))

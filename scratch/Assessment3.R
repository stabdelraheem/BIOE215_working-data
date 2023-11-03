library(tidyverse)

#Read and inspect data
bor_nestlings <- read_csv("data/73_species.csv")
bor_traits <- read_csv("data/Traits_73_species.csv")
avonet <- readxl::read_excel("data/AVONET%20Supplementary%20dataset%201.xlsx")

#Are nestlings showing up earlier in the year over time? 
all_birds_trend <- bor_nestlings %>% 
  group_by(Year) %>% 
  summarize(mean_doy = mean(Dayofyear))

nestling_trendsPlot <- ggplot(all_birds_trend, aes(Year, mean_doy)) + 
  geom_point() +
  geom_smooth(method = "lm")

#Species breakdown
species_trends <- bor_nestlings %>% 
  group_by(Year, Species) %>% 
  summarize(mean_doy = mean(Dayofyear),
            .groups = "drop")

spp_trendsPlot <- ggplot(species_trends, aes(Year, mean_doy, color = Species)) + 
  geom_point() +
  geom_smooth(method = "lm")

#Too much, now trying to pull data for 5 most data-rich spp

data_richness <- bor_nestlings %>% 
  count(Species)

most_rich <- data_richness %>% 
  arrange(desc(n)) %>% 
  slice(1:5)

most_rich_trends <- bor_nestlings %>% 
  filter(Species %in% most_rich$Species) %>% 
  group_by(Species, Year) %>% 
  summarize(mean_doy = mean(Dayofyear), 
            .groups = "drop")

top5_trendsPlot <- ggplot(most_rich_trends, aes(Year, mean_doy, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm")

# MAX CODE: Finds the slope of the relationship between y and x
trend <- function(x, y) {
  xy_lm <- lm(y ~ x)
  coef(xy_lm)[2]
}

# Calculate the trend for all species
bor_trends <- species_trends %>% 
  group_by(Species) %>% 
  summarize(doy_trend = trend(Year, mean_doy))

# Spot check two of the five top spp

soi <- c("ARDCIN", "LARMIN")
species_trends %>% 
  filter(Species %in% soi) %>% 
  ggplot(aes(Year, mean_doy, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm")



nrow_bor_trends <- nrow(bor_trends) # Use this later
bor_extreme <- bor_trends %>% 
  # Sort by the day of year trend
  trend(___) %>% 
  # Keep just the first (most negative trend) and last (most positive trend) rows
  slice(c(___, ___))

# Now plot them
species_trends %>% 
  filter(Species %in% ___) %>% 
  ggplot(aes(Year, mean_doy, color = Species)) + 
  geom_point() +
  geom_smooth(method = "lm")
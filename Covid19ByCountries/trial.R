library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)

# Import data from csv
Rawfile <- read.csv("owid-covid-data.csv")


# Load buck data from many countries and clean data
ByCountry <- Rawfile %>%
  select(c(continent, location, total_cases, 
           population, population_density)) %>%
  mutate(total_cases=if_else(is.na(total_cases), 0, total_cases)) %>%
  filter(!is.na(population)) %>%
  filter(!is.na(population_density)) %>%
  group_by(location) %>%
  summarise(continent, location,
            infect_rate=round(max(total_cases)/population*100, 2),
            density=max(population_density)) %>%
  filter(density < 1000) %>%
  distinct(location, .keep_all=TRUE)


ACountry <- ByCountry
# ACountry <- ByCountry %>% filter(continent=="Europe")
# ACountry <- ByCountry %>% filter(continent=="Asia")
# ACountry <- ByCountry %>% filter(continent=="African")

ggplot(ACountry, aes(y=infect_rate, x=density), .xlim=c(0, 1)) +
  geom_point(shape=18, color="blue") +
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue")

glimpse(ByCountry)


plot(ByCountry$infect_rate, ByCountry$density)
abline(lm (ByCountry$density ~ ByCountry$infect_rate))
# abline(472.606, -6.025)

# watcher <- ByCountry %>%
#   summarise(dependent=infect_rate/density)
#   
# 
# watcher %>%
#   select(c(dependent)) %>%
#   gghistogram(x = "dependent", .binwidth=0.05, xlim=c(0, 0.25),
#             fill = "#0073C2FF", color = "#0073C2FF",
#             add = "mean", rug = TRUE)


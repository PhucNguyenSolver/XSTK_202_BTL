library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)

# Import data from csv
Rawfile <- read.csv("owid-covid-data.csv")
glimpse(Rawfile)

ByCountry <- Rawfile %>%
  select(c(continent, location, total_cases, total_deaths, life_expectancy)) %>%
  mutate(total_cases =if_else(is.na(total_cases),  0, total_cases)) %>%
  mutate(total_deaths=if_else(is.na(total_deaths), 0, total_deaths)) %>%
  group_by(location) %>%
  mutate(total_cases=max(total_cases)) %>%
  mutate(total_deaths=max(total_deaths)) %>%
  mutate(death_rate = round(total_deaths/total_cases*100, 2)) %>%
  mutate(total_cases=NULL, total_deaths=NULL) %>%
  filter(continent != "") %>%
  filter(!is.na(death_rate)) %>%
  filter(!is.na(life_expectancy)) %>%
  distinct(location, .keep_all=TRUE)


Classes <- ByCountry
Classes$death_rate <- cut(Classes$death_rate,
                         breaks=c(-Inf, 1.5, 4.5, Inf),
                         labels=c("Positive", "Neutral", "Negative"),
                         )
Classes$life_expectancy <- cut(Classes$life_expectancy,
                          breaks=c(-Inf, 65, 75, Inf),
                          labels=c("Medium", "High", "Very high"),
)

write.csv(Classes, "data/classification/trial1.csv")



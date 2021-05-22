library(dplyr)
library(stringr)
library(ggplot2)

library(ggplot2)
library(ggpubr)
# Import data from csv
Rawfile <- read.csv("owid-covid-data.csv")

# Load buck data from many countries 
Countries <- Rawfile %>%
  select (c(location, date, total_cases, new_cases, new_cases_smoothed, 
            total_deaths, new_deaths, new_deaths_smoothed, reproduction_rate,
            total_tests, total_tests_per_thousand, population_density))

SimpleCountries <- Rawfile %>%
  select (c(location, date, total_cases, new_cases, total_deaths, new_deaths))

glimpse(Countries)
glimpse(SimpleCountries)


# Filter for specific_country and cleaning data
specific_country <- 'United Kingdom'
SimpleVietnam <- SimpleCountries %>%
  filter (location == specific_country) %>%
  arrange(date)

SimpleVietnam[is.na(SimpleVietnam)] <- 0

glimpse(SimpleVietnam)


# Attempt to group data weekly
weekly <- SimpleVietnam

id <- rownames(weekly)
weekly <- cbind(id=as.numeric(id), weekly)
glimpse(weekly)

WeeklyVietnam <- weekly %>%
  mutate(week=id%/%7) %>%
  group_by(week) %>%
  summarise(week, weekly_cases=sum(new_cases), weekly_deaths=sum(new_deaths)) %>%
  arrange(week) %>%
  mutate(deathrate = ifelse(weekly_cases == 0, 0, weekly_deaths / weekly_cases)) %>%
  distinct(week, .keep_all=TRUE)
glimpse(WeeklyVietnam)

# TODO: collect this script into function: 
#     [df] GetWeeklyData(string country)

wdata <- WeeklyVietnam%>%
  select(c(deathrate))
# gghistogram(wdata, x = "deathrate", bins = 30, 
#             fill = "#0073C2FF", color = "#0073C2FF",
#             add = "mean", rug = TRUE)
ggdensity(wdata, x = "deathrate", 
          fill = "#0073C2FF", color = "#0073C2FF",
          add = "mean", rug = TRUE)
  
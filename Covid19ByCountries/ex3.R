library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)

# Import data from csv
Rawfile <- read.csv("owid-covid-data.csv")


# Load buck data from many countries and clean data
TinyCountries <- Rawfile %>%
  select (c(continent, location, total_cases, total_deaths))
TinyCountries[is.na(TinyCountries)] <- 0
glimpse(TinyCountries)

# Calculate average death_rate and filter for each country 
ByCountry <- TinyCountries %>%
  group_by(location) %>%
  summarise(continent, location, all_deaths=max(total_deaths),
                                 all_cases = max(total_cases)) %>%
  distinct(location, .keep_all=TRUE) %>%
  filter(continent!="") %>%
  mutate(death_rate = ifelse(all_cases == 0, 
                             0, 
                             round(100 * all_deaths / all_cases, 2)))
  
# Split by continent
ByContinent <- split(ByCountry, ByCountry$continent)
a <- ByContinent$Africa # <-- put continent here
b <- ByContinent$Asia # <-- put continent here
c <- ByContinent$Europe # <-- put continent here
d <- ByContinent$'North America' # <-- put continent here

lenMax <- max(length(a[[1]]), length(b[[1]]), length(c[[1]]), length(d[[1]]))
All <- data.frame(Africa = c(a$death_rate, rep("", lenMax - length(a[[1]]))),
                  Asia   = c(b$death_rate, rep("", lenMax - length(b[[1]]))), 
                  Europe = c(c$death_rate, rep("", lenMax - length(c[[1]]))),
                  South_America = c(d$death_rate, rep("", lenMax - length(d[[1]]))))

write.csv(a, "data/DeathRate/Africa.csv")
write.csv(b, "data/DeathRate/Asia.csv")
write.csv(c, "data/DeathRate/Europe.csv")
write.csv(d, "data/DeathRate/North_America.csv")
write.csv(All, "data/DeathRate/ByContinents.csv")

# All %>%
#   select(c(death_rate)) %>%
#   gghistogram(x = "death_rate", bins = 25,
#             fill = "#0073C2FF", color = "#0073C2FF",
#             add = "mean", rug = TRUE)


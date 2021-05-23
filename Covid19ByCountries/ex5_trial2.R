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
                          labels=c("Trung bình", "Cao", "Rất cao"),
)

write.csv(Classes, "data/classification/trial1.csv")
# ACountry <- ByCountry
# ACountry <- ByCountry %>% filter(continent=="Europe")
# ACountry <- ByCountry %>% filter(continent=="Asia")
# ACountry <- ByCountry %>% filter(continent=="African")

# ggplot(ACountry, aes(y=infect_rate, x=density), .xlim=c(0, 1)) +
#   geom_point(shape=18, color="blue") +
#   geom_smooth(method=lm,  linetype="dashed",
#               color="darkred", fill="blue")
# 
# glimpse(ByCountry)


# plot(ByCountry$infect_rate, ByCountry$density)
# abline(lm (ByCountry$density ~ ByCountry$infect_rate))
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


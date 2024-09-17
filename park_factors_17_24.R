library(readxl)
library(tidyverse)
library(ggplot2)
library(baseballr)
library(data.table)

#grabbing park factors
pf1 <- baseballr::fg_park(2024)
pf2 <- baseballr::fg_park(2023)
pf3 <- baseballr::fg_park(2022)
pf4 <- baseballr::fg_park(2021)
pf5 <- baseballr::fg_park(2020)
pf6 <- baseballr::fg_park(2019)
pf7 <- baseballr::fg_park(2018)
pf8 <- baseballr::fg_park(2017)

pf <- rbind(pf1, pf2, pf3, pf4, pf5, pf6, pf7, pf8)


team_mapping <- c(
  "Yankees" = "New York Yankees",
  "Red Sox" = "Boston Red Sox",
  "Orioles" = "Baltimore Orioles",
  "Blue Jays" = "Toronto Blue Jays",
  "Rays" = "Tampa Bay Rays",
  "Athletics" = "Oakland Athletics",
  "Angels" = "Los Angeles Angels",
  "Mariners" = "Seattle Mariners",
  "Rangers" = "Texas Rangers",
  "Astros" = "Houston Astros",
  "Nationals" = "Washington Nationals",
  "Mets" = "New York Mets",
  "Braves" = "Atlanta Braves",
  "Phillies" = "Philadelphia Phillies",
  "Marlins" = "Miami Marlins",
  "Brewers" = "Milwaukee Brewers",
  "Cubs" = "Chicago Cubs",
  "Cardinals" = "St. Louis Cardinals",
  "Pirates" = "Pittsburgh Pirates",
  "Reds" = "Cincinnati Reds",
  "White Sox" = "Chicago White Sox",
  "Twins" = "Minnesota Twins",
  "Royals" = "Kansas City Royals",
  "Tigers" = "Detroit Tigers",
  "Guardians" = "Cleveland Guardians",
  "Giants" = "San Francisco Giants",
  "Padres" = "San Diego Padres",
  "Dodgers" = "Los Angeles Dodgers",
  "Diamondbacks" = "Arizona Diamondbacks",
  "Rockies" = "Colorado Rockies"
)

# Ensure 'season' column is integer
pf$season <- as.integer(pf$season)

pf$home_team <- team_mapping[pf$home_team]



#save
saveRDS(pf, "C:/Users/dresi/Documents/all_park_factors.rds")

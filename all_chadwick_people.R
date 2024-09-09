library(tidyverse)
library(ggplot2)
library(baseballr)
library(data.table)

#acquring all people from Chadwick

url1 <- "https://github.com/chadwickbureau/register/raw/master/data/people-0.csv"
p1 <- read.csv(url1)

url2 <- "https://github.com/chadwickbureau/register/raw/master/data/people-1.csv"
p2 <- read.csv(url2)

players <- rbind(p1, p2)

url3 <- "https://github.com/chadwickbureau/register/raw/master/data/people-2.csv"
p3 <- read.csv(url3)

players <- rbind(players, p3)

url4 <- "https://github.com/chadwickbureau/register/raw/master/data/people-3.csv"
p4 <- read.csv(url4)

players <- rbind(players, p4)

url5 <- "https://github.com/chadwickbureau/register/raw/master/data/people-4.csv"
p5 <- read.csv(url5)

players <- rbind(players, p5)

url6 <- "https://github.com/chadwickbureau/register/raw/master/data/people-5.csv"
p6 <- read.csv(url6)

players <- rbind(players, p6)

url7 <- "https://github.com/chadwickbureau/register/raw/master/data/people-6.csv"
p7 <- read.csv(url7)

players <- rbind(players, p7)

url8 <- "https://github.com/chadwickbureau/register/raw/master/data/people-7.csv"
p8 <- read.csv(url8)

players <- rbind(players, p8)

url9 <- "https://github.com/chadwickbureau/register/raw/master/data/people-8.csv"
p9 <- read.csv(url9)

players <- rbind(players, p9)

url10 <- "https://github.com/chadwickbureau/register/raw/master/data/people-9.csv"
p10 <- read.csv(url10)

players <- rbind(players, p10)

url11 <- "https://github.com/chadwickbureau/register/raw/master/data/people-a.csv"
p11 <- read.csv(url11)

players <- rbind(players, p11)

url12 <- "https://github.com/chadwickbureau/register/raw/master/data/people-b.csv"
p12 <- read.csv(url12)

players <- rbind(players, p12)

url13 <- "https://github.com/chadwickbureau/register/raw/master/data/people-c.csv"
p13 <- read.csv(url13)

players <- rbind(players, p13)

url14 <- "https://github.com/chadwickbureau/register/raw/master/data/people-d.csv"
p14 <- read.csv(url14)

players <- rbind(players, p14)

url15 <- "https://github.com/chadwickbureau/register/raw/master/data/people-e.csv"
p15 <- read.csv(url15)

players <- rbind(players, p15)

url16 <- "https://github.com/chadwickbureau/register/raw/master/data/people-f.csv"
p16 <- read.csv(url16)

players <- rbind(players, p16)

#replace all blank values in df with NA
players[players == ''] <- NA

# Keep rows where any of key_mlbam, key_bbref, or key_bbref_minors is not NA
players <- players |> filter(!is.na(key_mlbam) | !is.na(key_bbref) | !is.na(key_bbref_minors)
                             | !is.na(pro_umpired_first) | !is.na(pro_umpired_last) | !is.na(mlb_umpired_first)
                             | !is.na(mlb_umpired_last))

#get rid of non-baseball sports
players <- players |> filter(is.na(players$key_sr_nba) | is.na(players$key_sr_nfl)
                             | is.na(players$key_sr_nhl) | is.na(players$key_sr_nba))

#ensures we don't remove umpires
players <- players |> filter(birth_year >= '1950' | is.na(birth_year))

saveRDS(players, file = "people.rds")

library(baseballr)
library(tidyverse)
library(zoo)
library(data.table)
library(ggplot2)
library(httr)


#load in all necessary RDS files as data frames
all_data <- readRDS("mlb_master_data.rds")
p_by_game <- readRDS("all_pitcher_game_by_game_averages.rds")
lg_sp_avg_yr <- readRDS("league_starting_pitching_averages.rds")
team_bat_by_szn <- readRDS("team_batting_by_season.rds")
team_gbg_avg <- readRDS("team_rolling_game_averages.rds")
p_by_szn <- readRDS("player_pitching_stats_by_season.rds")




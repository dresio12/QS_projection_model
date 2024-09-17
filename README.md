Quality Start Projection Model (Work in Progress)

Currently scraping and organizaing the data needed to generate a model to predict the probability of a quality start.

Model variables planned to include: season-long and recent (~6 starts) performance, opposing team season long and recent (~10 games) performance/record, weather, park factor, pitcher handedness, HP umpire

The end goal is to have a self-updating model available to view on a Shiny application

###

DESCRIPTION OF FILES

MLB_data_excel_file_creation.R: Work in progress script that has gone under a few changes. Generates the game_pack data files, game information data files with starting pitcher and umpire information, and generates the Chadwick people file to be used in the cumulative stats script

park_factors_17_24.R: obtains all the park factors for each stadium from 2017-2024

all_chadwick_people.R: used to obtain the MLBAM and FangraphsID for new players when available

cumulative_stats_for_QS_model:  Creates 5 RDS files
  1) DF for all pitcher game logs from 2017-2024 with various additional cumualtive and 6-game split rolling average stats
  2) DF for all pitcher season-long stats and averages to fill in game 1 of the season cumulative and rolling averages (model variables) in future use 
  3) DF for the league average cumulative and rolling stats to be used for first games of the season information for rookie/returning-from-injury players in place of previous year season avgs
    - does not need rolling averages since the for the first game of the season there aren't any   
  4) DF for team cumulative and rolling stats by game log in the same structure and pitcher game logs
  5) DF for team seasonal averages to be used to fill in game 1 of the season cumulative and rolling averages (model variables) in future use 

weather_data.R: a work in progress script to pull pre-game weather info for use in obtaining weather-related model variables


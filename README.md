Quality Start Projection Model (Work in Progress)

Currently scraping and organizaing the data needed to generate a model to predict the probability of a quality start.

Model variables planned to include: season-long and recent (~6 starts) performance, opposing team season long and recent (~10 games) performance/record, weather, park factor, pitcher handedness

The end goal is to have a self-updating model available to view on a Shiny application

###

DESCRIPTION OF FILES

MLB_data_excel_file_creation.R: The main Rscript to generate the master dataframe. Very much currently a work in progress.

park_factors_17_24: obtains all the park factors for each stadium from 2017-2024

game_pks_df: obtain the game packs (game_pk) that serve as the unique identifier of each game

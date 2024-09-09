Quality Start Projection Model (Work in Progress)

Currently scraping and organizaing the data needed to generate a model to predict the probability of a quality start.

Model variables planned to include: season-long and recent (~6 starts) performance, opposing team season long and recent (~10 games) performance/record, weather, park factor, pitcher handedness, HP umpire

The end goal is to have a self-updating model available to view on a Shiny application

###

DESCRIPTION OF FILES

MLB_data_excel_file_creation.R: The main Rscript to generate the master dataframe. Very much currently a work in progress.

park_factors_17_24.R: obtains all the park factors for each stadium from 2017-2024

all_chadwick_people.R: obtain all the necessary players and umpires for this model and potentially future others.

game_packs_df.rds: the game packs used to create the initial model, does not update daily

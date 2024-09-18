library(readxl)
library(tidyverse)
library(ggplot2)
library(baseballr)
library(data.table)
library(rvest)
library(stringr)
library(googlesheets4)

#load in all_data with game packs
all_data <- readRDS('mlb_master_data.rds')

# Initialize an empty list to store starter information
starter_list <- list()

get_game_starter <- function(game_pk) {
  tryCatch({
    game_starter <- baseballr::get_probables_mlb(game_pk = game_pk)
    return(game_starter)
  }, error = function(e) {
    message("Error retrieving starter for game_pk ", game_pk, ": ", e$message)
    return(NULL)
  })
}

# Loop through the filtered game_pks
for (i in seq_along(all_data$game_pk)) {
  game_pk <- all_data$game_pk[i]
  
  # Retrieve the game starter
  starter_list[[i]] <- get_game_starter(game_pk)
}

starter_list_df <- rbindlist(starter_list, fill = TRUE)

saveRDS(starter_list_df, 'starter_list.rds')


#join to all_data by game_pk
all_data <- left_join(all_data, starter_list_df, by = c('game_pk' = 'game_pk',
                                                        'officialDate' = 'game_date'))

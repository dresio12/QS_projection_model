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

set.seed(123)


# Load the existing game_packs_df (if saved as a file)
if (file.exists("mlb_master_data.rds")) {
  all_data <- readRDS("mlb_master_data.rds")
} else {
  all_data <- data.table()  # Create an empty dataframe if not available
}

# Get today's date
today_date <- Sys.Date() - 1

# Retrieve the game packs for today's date
new_game_packs <- baseballr::get_game_pks_mlb(date = today_date, level_ids = 1)

# Combine the new data with the existing game_packs_df
all_data <- rbindlist(list(all_data, new_game_packs), fill = TRUE)

#retrieve yesterday game_pks
today_date <- Sys.Date() -1

# Define a function to retrieve game info for a given game_pk
get_game_info_for_game <- function(game_pk) {
  tryCatch({
    game_info <- baseballr::get_game_info_mlb(game_pk = game_pk)
    return(game_info)
  }, error = function(e) {
    message("Error retrieving game info for game_pk ", game_pk, ": ", e$message)
    return(NULL)
  })
}

new_game_info_list <- lapply(new_game_packs$game_pk, get_game_info_for_game)
combined_game_info2024 <- rbindlist(c(new_game_info_list), fill = TRUE)

#convert new game packs temp into integer
combined_game_info2024$temperature <- as.integer(combined_game_info2024$temperature)

# Perform a left join to add columns from combined_game_info2024 to all_data
all_data <- left_join(all_data, combined_game_info2024, by = "game_pk")

# Replace the specific columns
all_data <- all_data %>%
  mutate(
    temperature.x = coalesce(temperature.y, temperature.x),
    other_weather.x = coalesce(other_weather.y, other_weather.x),
    wind.x = coalesce(wind.y, wind.x)
  ) %>%
  # Drop the .y columns
  select(-ends_with(".y")) %>%
  # Rename the .x columns to remove the .x suffix
  rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))

all_data <- all_data |>
  select(1:43, 252:257, 239, 242:247, 249, 261:263, 268)


#add updated park factors
pf <- baseballr::fg_park(2024)

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
all_data$season <- as.integer(all_data$season)

pf$home_team <- team_mapping[pf$home_team]

all_data <- left_join(all_data, pf, by = c("season", "teams.home.team.name" = "home_team"))

# Replace the specific columns
all_data <- all_data %>%
  mutate(
    `1yr.x` = coalesce(`1yr.y`, `1yr.x`),
    `3yr.x` = coalesce(`3yr.y`, `3yr.x`)) %>%
  # Drop the .y columns
  select(-ends_with(".y")) %>%
  # Rename the .x columns to remove the .x suffix
  rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))

all_data <- all_data |>
  select(1:43)

all_data <- all_data |>
  arrange(season, gameDate)


# Save the updated dataframe
saveRDS(all_data, file = "mlb_master_data.rds")

######

#add in probable pitchers for the day

# Convert the current system date to character
current_date <- as.character(Sys.Date())

# Subset the dataframe to include only rows with the current date in all_data$officialDate
all_data_subset <- all_data %>% 
  filter(as.character(officialDate) == current_date)

get_game_starter <- function(game_pk) {
  tryCatch({
    game_starter <- baseballr::get_probables_mlb(game_pk = game_pk)
    return(game_starter)
  }, error = function(e) {
    message("Error retrieving starter for game_pk ", game_pk, ": ", e$message)
    return(NULL)
  })
}


# Initialize an empty list to store starter information
starter_list <- list()

# Loop through the filtered game_pks
for (i in seq_along(all_data_subset$game_pk)) {
  game_pk <- all_data_subset$game_pk[i]
  
  # Retrieve the game starter
  starter_list[[i]] <- get_game_starter(game_pk)
}

# Combine results into a single dataframe
starter_list_df <- rbindlist(starter_list, fill = TRUE)

starter_list_df <- starter_list_df %>%
  group_by(game_pk, game_date, home_plate_full_name, home_plate_id) %>%
  summarize(
    away_starter = first(fullName),
    id1 = first(id),
    teams.away.team.name = first(team),
    teams.away.team.id = first(team_id),
    home_starter = last(fullName),
    id2 = last(id),
    teams.home.team.name = last(team),
    teams.home.team.id = last(team_id),
    .groups = 'drop'
  )

# Join the processed data back to all_data
all_data <- left_join(all_data, starter_list_df, by = c('game_pk', 
                                                        "teams.home.team.name",
                                                        "teams.away.team.name",
                                                        "teams.home.team.id",
                                                        "teams.away.team.id"))

# Replace the specific columns only if they are NA in all_data
all_data <- all_data %>%
  mutate(
    game_date.x = coalesce(game_date.y, game_date.x),
    home_plate_full_name.x = coalesce(home_plate_full_name.y, home_plate_full_name.x),
    home_plate_id.x = coalesce(home_plate_id.y, home_plate_id.x),
    away_starter.x = coalesce(away_starter.y, away_starter.x),
    id1.x = coalesce(id1.y, id1.x),
    home_starter.x = coalesce(home_starter.y, home_starter.x),
    id2.x = coalesce(id2.y, id2.x)) %>%
  # Drop the .y columns
  select(-ends_with(".y")) %>%
  # Rename the .x columns to remove the .x suffix
  rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))

# Save the updated dataframe
saveRDS(all_data, file = "mlb_master_data.rds")






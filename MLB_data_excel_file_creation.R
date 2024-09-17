library(readxl)
library(tidyverse)
library(ggplot2)
library(baseballr)
library(data.table)
library(rvest)
library(stringr)
library(googlesheets4)

#turn off scientific notation

options(scipeb = 999)

#example of grabbing single-game information at MLB level
#For MiLB, use different levelID

game_packs <- baseballr::get_game_pks_mlb(date = '2024-09-05', level_ids = 1)

#Grabbing the whole season

# Define the start and end dates

start_date <- as.Date('2017-03-15')
end_date <- as.Date('2024-09-06')

# Generate the sequence of dates

dates <- seq.Date(from = start_date, to = end_date, by = "day")

# Get game packs for all the dates

game_packs <- lapply(dates, function(date) {
  baseballr::get_game_pks_mlb(date = date, level_ids = 1)
})

# Combine the results into a single dataframe using rbindlist with fill=TRUE

game_packs_df <- rbindlist(game_packs, fill = TRUE)

mlb_master_data <- game_packs_df |> select(-dates)
#NA values correspond to rained out games that need rescheduling

#remove spring training and other exhibitions
mlb_master_data <- mlb_master_data |>
  filter(gameType == "R")

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

#
s2017 <- mlb_master_data |> filter(season == '2017')
s2017a <- s2017[1:308, ]
s2017b <- s2017[309:618, ]
s2017c <- s2017[619:928, ]
s2017d <- s2017[929:1238, ]
s2017e <- s2017[1239:1548, ]
s2017f <- s2017[1549:1858, ]
s2017g <- s2017[1859:2168, ]
s2017h <- s2017[2169:2469, ]

s2018 <- mlb_master_data |> filter(season == '2018')
s2018a <- s2018[1:308, ]
s2018b <- s2018[309:618, ]
s2018c <- s2018[619:928, ]
s2018d <- s2018[929:1238, ]
s2018e <- s2018[1239:1548, ]
s2018f <- s2018[1549:1858, ]
s2018g <- s2018[1859:2168, ]
s2018h <- s2018[2169:2487, ]

s2019 <- mlb_master_data |> filter(season == '2019')
s2019a <- s2019[1:308, ]
s2019b <- s2019[309:618, ]
s2019c <- s2019[619:928, ]
s2019d <- s2019[929:1238, ]
s2019e <- s2019[1239:1548, ]
s2019f <- s2019[1549:1858, ]
s2019g <- s2019[1859:2168, ]
s2019h <- s2019[2169:2472, ]

s2020 <- mlb_master_data |> filter(season == '2020')
s2020a <- s2020[1:486, ]
s2020b <- s2020[487:973, ]

s2021 <- mlb_master_data |> filter(season == '2021')
s2021a <- s2021[1:308, ]
s2021b <- s2021[309:618, ]
s2021c <- s2021[619:928, ]
s2021d <- s2021[929:1238, ]
s2021e <- s2021[1239:1548, ]
s2021f <- s2021[1549:1858, ]
s2021g <- s2021[1859:2168, ]
s2021h <- s2021[2169:2512, ]

s2022 <- mlb_master_data |> filter(season == '2022')
s2022a <- s2022[1:308, ]
s2022b <- s2022[309:618, ]
s2022c <- s2022[619:928, ]
s2022d <- s2022[929:1238, ]
s2022e <- s2022[1239:1548, ]
s2022f <- s2022[1549:1858, ]
s2022g <- s2022[1859:2168, ]
s2022h <- s2022[2169:2479, ]

s2023 <- mlb_master_data |> filter(season == '2023')
s2023a <- s2023[1:308, ]
s2023b <- s2023[309:618, ]
s2023c <- s2023[619:928, ]
s2023d <- s2023[929:1238, ]
s2023e <- s2023[1239:1548, ]
s2023f <- s2023[1549:1858, ]
s2023g <- s2023[1859:2168, ]
s2023h <- s2023[2169:2476, ]


s2024 <- mlb_master_data |> filter(season == '2024')
s2024a <- s2024[1:308, ]
s2024b <- s2024[309:618, ]
s2024c <- s2024[619:928, ]
s2024d <- s2024[929:1238, ]
s2024e <- s2024[1239:1548, ]
s2024f <- s2024[1549:1858, ]
s2024g <- s2024[1859:2157, ]


# Retrieve game info for all game_pks
game_info_list1 <- lapply(s2017a$game_pk, get_game_info_for_game)
game_info_list2 <- lapply(s2017b$game_pk, get_game_info_for_game)
game_info_list3 <- lapply(s2017c$game_pk, get_game_info_for_game)
game_info_list4 <- lapply(s2017d$game_pk, get_game_info_for_game)
game_info_list5 <- lapply(s2017e$game_pk, get_game_info_for_game)
game_info_list6 <- lapply(s2017f$game_pk, get_game_info_for_game)
game_info_list7 <- lapply(s2017g$game_pk, get_game_info_for_game)
game_info_list8 <- lapply(s2017h$game_pk, get_game_info_for_game)
combined_game_info2017 <- rbindlist(c(game_info_list1,
                                      game_info_list2,
                                      game_info_list3,
                                      game_info_list4,
                                      game_info_list5,
                                      game_info_list6,
                                      game_info_list7,
                                      game_info_list8), fill = TRUE)

game_info_list9 <- lapply(s2018a$game_pk, get_game_info_for_game)
game_info_list10 <- lapply(s2018b$game_pk, get_game_info_for_game)
game_info_list11 <- lapply(s2018c$game_pk, get_game_info_for_game)
game_info_list12 <- lapply(s2018d$game_pk, get_game_info_for_game)
game_info_list13 <- lapply(s2018e$game_pk, get_game_info_for_game)
game_info_list14 <- lapply(s2018f$game_pk, get_game_info_for_game)
game_info_list15 <- lapply(s2018g$game_pk, get_game_info_for_game)
game_info_list16 <- lapply(s2018h$game_pk, get_game_info_for_game)
combined_game_info2018 <- rbindlist(c(game_info_list9,
                                      game_info_list10,
                                      game_info_list11,
                                      game_info_list12,
                                      game_info_list13,
                                      game_info_list14,
                                      game_info_list15,
                                      game_info_list16), fill = TRUE)

game_info_list17 <- lapply(s2019a$game_pk, get_game_info_for_game)
game_info_list18 <- lapply(s2019b$game_pk, get_game_info_for_game)
game_info_list19 <- lapply(s2019c$game_pk, get_game_info_for_game)
game_info_list20 <- lapply(s2019d$game_pk, get_game_info_for_game)
game_info_list21 <- lapply(s2019e$game_pk, get_game_info_for_game)
game_info_list22 <- lapply(s2019f$game_pk, get_game_info_for_game)
game_info_list23 <- lapply(s2019g$game_pk, get_game_info_for_game)
game_info_list24 <- lapply(s2019h$game_pk, get_game_info_for_game)
combined_game_info2019 <- rbindlist(c(game_info_list17,
                                      game_info_list18,
                                      game_info_list19,
                                      game_info_list20,
                                      game_info_list21,
                                      game_info_list22,
                                      game_info_list23,
                                      game_info_list24), fill = TRUE)

game_info_list25 <- lapply(s2020a$game_pk, get_game_info_for_game)
game_info_list26 <- lapply(s2020b$game_pk, get_game_info_for_game)
combined_game_info2020 <- rbindlist(c(game_info_list25,
                                      game_info_list26), fill = TRUE)

game_info_list27 <- lapply(s2021a$game_pk, get_game_info_for_game)
game_info_list28 <- lapply(s2021b$game_pk, get_game_info_for_game)
game_info_list29 <- lapply(s2021c$game_pk, get_game_info_for_game)
game_info_list30 <- lapply(s2021d$game_pk, get_game_info_for_game)
game_info_list31 <- lapply(s2021e$game_pk, get_game_info_for_game)
game_info_list32 <- lapply(s2021f$game_pk, get_game_info_for_game)
game_info_list33 <- lapply(s2021g$game_pk, get_game_info_for_game)
game_info_list34 <- lapply(s2021h$game_pk, get_game_info_for_game)
combined_game_info2021 <- rbindlist(c(game_info_list27,
                                      game_info_list28,
                                      game_info_list29,
                                      game_info_list30,
                                      game_info_list31,
                                      game_info_list32,
                                      game_info_list33,
                                      game_info_list34), fill = TRUE)

game_info_list35 <- lapply(s2022a$game_pk, get_game_info_for_game)
game_info_list36 <- lapply(s2022b$game_pk, get_game_info_for_game)
game_info_list37 <- lapply(s2022c$game_pk, get_game_info_for_game)
game_info_list38 <- lapply(s2022d$game_pk, get_game_info_for_game)
game_info_list39 <- lapply(s2022e$game_pk, get_game_info_for_game)
game_info_list40 <- lapply(s2022f$game_pk, get_game_info_for_game)
game_info_list41 <- lapply(s2022g$game_pk, get_game_info_for_game)
game_info_list42 <- lapply(s2022h$game_pk, get_game_info_for_game)
combined_game_info2022 <- rbindlist(c(game_info_list35,
                                      game_info_list36,
                                      game_info_list37,
                                      game_info_list38,
                                      game_info_list39,
                                      game_info_list40,
                                      game_info_list41,
                                      game_info_list42), fill = TRUE)

game_info_list43 <- lapply(s2023a$game_pk, get_game_info_for_game)
game_info_list44 <- lapply(s2023b$game_pk, get_game_info_for_game)
game_info_list45 <- lapply(s2023c$game_pk, get_game_info_for_game)
game_info_list46 <- lapply(s2023d$game_pk, get_game_info_for_game)
game_info_list47 <- lapply(s2023e$game_pk, get_game_info_for_game)
game_info_list48 <- lapply(s2023f$game_pk, get_game_info_for_game)
game_info_list49 <- lapply(s2023g$game_pk, get_game_info_for_game)
game_info_list50 <- lapply(s2023h$game_pk, get_game_info_for_game)
combined_game_info2023 <- rbindlist(c(game_info_list43,
                                      game_info_list44,
                                      game_info_list45,
                                      game_info_list46,
                                      game_info_list47,
                                      game_info_list48,
                                      game_info_list49,
                                      game_info_list50), fill = TRUE)

game_info_list51 <- lapply(s2024a$game_pk, get_game_info_for_game)
game_info_list52 <- lapply(s2024b$game_pk, get_game_info_for_game)
game_info_list53 <- lapply(s2024c$game_pk, get_game_info_for_game)
game_info_list54 <- lapply(s2024d$game_pk, get_game_info_for_game)
game_info_list55 <- lapply(s2024e$game_pk, get_game_info_for_game)
game_info_list56 <- lapply(s2024f$game_pk, get_game_info_for_game)
game_info_list57 <- lapply(s2024g$game_pk, get_game_info_for_game)
combined_game_info2024 <- rbindlist(c(game_info_list51,
                                      game_info_list52,
                                      game_info_list53,
                                      game_info_list54,
                                      game_info_list55,
                                      game_info_list56,
                                      game_info_list57), fill = TRUE)



# Combine results into a single dataframe
all_game_info_df <- rbind(combined_game_info2017,
                          combined_game_info2018,
                          combined_game_info2019,
                          combined_game_info2020,
                          combined_game_info2021,
                          combined_game_info2022,
                          combined_game_info2023,
                          combined_game_info2024)


mlb_master_data <- left_join(mlb_master_data, all_game_info_df, by = "game_pk")
mlb_master_data <- unique(mlb_master_data)

saveRDS(mlb_master_data,  "C:/Users/dresi/Documents/mlb_master_data.rds")


# Define a function to retrieve pitcher for a given game_pk
get_game_starter <- function(game_pk) {
  tryCatch({
    game_starter <- baseballr::get_probables_mlb(game_pk = game_pk)
    return(game_starter)
  }, error = function(e) {
    message("Error retrieving starter for game_pk ", game_pk, ": ", e$message)
    return(NULL)
  })
}

# Retrieve game info for all game_pks
starter_list <- list()

# Load existing data if available
if (file.exists("starter_list.rds")) {
  starter_list <- readRDS("starter_list.rds")
  start_index <- length(starter_list) + 1  # Resume from the next unsaved game_pk
} else {
  starter_list <- list()
  start_index <- 1
}

# Loop through the remaining game_pks
for (i in seq(start_index, length(mlb_master_data$game_pk))) {
  game_pk <- mlb_master_data$game_pk[i]
  
  # Retrieve the game starter
  starter_list[[i]] <- get_game_starter(game_pk)
  
  # Save progress every 25 results
  if (i %% 25 == 0) {
    saveRDS(starter_list, "starter_list.rds")
    message(paste("Saved progress at game", i))
  }
}

# Final save to ensure all results are saved at the end
saveRDS(starter_list, "starter_list.rds")


# Combine results into a single dataframe
starter_list <- starter_list[-1042]

starter_list_df <- rbindlist(starter_list, fill = TRUE)

starter_list_df <- starter_list_df |>
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

saveRDS(starter_list_df, "probable_starts.rds")

mlb_master_data <- left_join(mlb_master_data, starter_list_df)

###

#add park factors from RDS
pf <- readRDS("all_park_factors.rds")

# Ensure master data 'season' column is integer
mlb_master_data$season <- as.integer(mlb_master_data$season)

# Perform the left join
all_data <- left_join(mlb_master_data, pf, 
                             by = c("teams.home.team.name" = "home_team", 
                                    "season"))

#trim to only necessary columns
all_data <- all_data |>
  select(1, 5:7, 16, 26:29, 33, 37:41, 43, 47:51, 53, 54, 58:61,
         67, 68, 71:76, 88:93, 95, 96)

saveRDS(all_data,  "C:/Users/dresi/Documents/mlb_master_data.rds")

###

#for some reason 2018 and 2018 didn't load into mlb_master_data
#this fixes that
all_data_subset <- all_data %>% 
  filter(season == '2017' | season == '2018')

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
  
  # Save progress every 25 results and overwrite existing file
  if (i %% 25 == 0) {
    saveRDS(starter_list, "starter_list.rds")
    message(paste("Saved progress at game", i))
  }
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


######

#Load in players DF to be able to extract necessary pitchers
players <- readRDS("people.rds")

# Extract unique player IDs from the 'id1' column
player_ids <- unique(c(all_data$id1, all_data$id2))


# Filter the players dataframe to find rows where players$key_mlbam matches player_ids
matched_df <- players %>%
  filter(key_mlbam %in% player_ids) %>%
  distinct(key_mlbam, .keep_all = TRUE)

matched_df <- matched_df |>
  filter(!is.na(key_mlbam))

matched_df <- matched_df %>%
  rename(playerid = key_fangraphs)
#some of the IDs result in NA in key_bbref. This is because they are rookies

#filter to get players who need ids
need_id <- matched_df |>
  filter(is.na(playerid))

#trim to necessary columns
need_id <- need_id |>
  select(key_mlbam, playerid, 13:14)

#using online resource that updates IDs
data <- read_sheet('1JgczhD5VDQ1EiXqVG-blttZcVwbZd5_Ne_mefUGwJnk')

#bring IDs over
need_id <- left_join(need_id, data, by = c('key_mlbam' = 'MLBID'))

need_id <- need_id |>
  select(1, 13)

# if `IDFANGRAPHS` is a list
if (is.list(need_id$IDFANGRAPHS)) {
  # Flatten the list and ensure correct length
  flattened_ids <- unlist(need_id$IDFANGRAPHS)
  flattened_ids <- as.character(flattened_ids)

  # Update the data frame
  need_id <- need_id[!is.na(flattened_ids), ]
  need_id$IDFANGRAPHS <- flattened_ids
}

need_id <- need_id %>%
  filter(str_detect(IDFANGRAPHS, "^\\d{5}$"))

# Convert the character vector to integer
need_id$IDFANGRAPHS <- as.integer(need_id$IDFANGRAPHS)

matched_df <- left_join(matched_df, need_id)

#fill NAs
matched_df <- matched_df |>
  mutate(playerid = ifelse(is.na(playerid), IDFANGRAPHS, playerid))


# Define the function to retrieve pitcher game logs
get_pitcher_logs <- function(playerid, year) {
  tryCatch({
    data <- fg_pitcher_game_logs(playerid = playerid, year = year)
    return(data)
  }, error = function(e) {
    message("Error retrieving data for playerid ", playerid, ": ", e$message)
    return(NULL)
  })
}

# Retrieve data for each player ID in the playerid column
pitcher_logs_list17 <- lapply(matched_df$playerid, function(pid) get_pitcher_logs(playerid = pid, year = 2017))
pitcher_logs_list18 <- lapply(matched_df$playerid, function(pid) get_pitcher_logs(playerid = pid, year = 2018))
pitcher_logs_list19 <- lapply(matched_df$playerid, function(pid) get_pitcher_logs(playerid = pid, year = 2019))
pitcher_logs_list20 <- lapply(matched_df$playerid, function(pid) get_pitcher_logs(playerid = pid, year = 2020))
pitcher_logs_list21 <- lapply(matched_df$playerid, function(pid) get_pitcher_logs(playerid = pid, year = 2021))
pitcher_logs_list22 <- lapply(matched_df$playerid, function(pid) get_pitcher_logs(playerid = pid, year = 2022))
pitcher_logs_list23 <- lapply(matched_df$playerid, function(pid) get_pitcher_logs(playerid = pid, year = 2023))
pitcher_logs_list24 <- lapply(matched_df$playerid, function(pid) get_pitcher_logs(playerid = pid, year = 2024))

all_pitcher_logs_lists <- list(pitcher_logs_list17, pitcher_logs_list18,
                               pitcher_logs_list19, pitcher_logs_list20,
                               pitcher_logs_list21, pitcher_logs_list22,
                               pitcher_logs_list23, pitcher_logs_list24)

# Flatten the list of lists
flattened_list <- do.call(c, all_pitcher_logs_lists)

# Convert all dataframes to data.tables
flattened_list <- lapply(flattened_list, as.data.table)

# Get all column names dynamically
all_columns <- unique(unlist(lapply(flattened_list, names)))

# Define function to add missing columns
add_missing_columns <- function(df, all_columns) {
  df <- as.data.table(df)  # Ensure it's a data.table
  missing_cols <- setdiff(all_columns, names(df))
  df[, (missing_cols) := NA]  # Add missing columns with NA values
  return(df)
}

# Align columns for each dataframe
aligned_dataframes <- lapply(flattened_list, function(df) {
  add_missing_columns(df, all_columns)
})

# Combine the data tables into a single data.table
combined_data <- rbindlist(aligned_dataframes, fill = TRUE)

# Remove rows with NA in specific columns (e.g., PlayerName)
combined_data <- combined_data[!is.na(PlayerName)]

# Save to RDS file
saveRDS(combined_data, file = "pitcher_game_logs.rds")


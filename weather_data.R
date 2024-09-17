library(httr)
library(rvest)
library(lubridate)

#testing with one stadium
api_key <- '33b05e38488f004530bad2e32eb60e34'

ARI <- GET(paste0("https://api.openweathermap.org/data/2.5/forecast?lat=33.4453&lon=-112.0667&units=imperial&appid=", api_key))

ARI_weather <- content(ARI, "parsed")

# Create a list of stadiums with their latitude and longitude
stadiums <- data.frame(
  team = c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", "HOU", "KC", 
           "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", "PHI", "PIT", "SD", "SEA", 
           "SF", "STL", "TB", "TEX", "TOR", "WSH"),
  latitude = c(33.4453, 33.8907, 39.2839, 42.3467, 41.9484, 41.8300, 39.0975, 41.4962, 39.7559, 
               42.3390, 29.7572, 39.0515, 33.8003, 34.0739, 25.7781, 43.0284, 44.9817, 40.7570, 
               40.8296, 37.7516, 39.9055, 40.4475, 32.7076, 47.5914, 37.7783, 38.6226, 27.7682, 
               32.7513, 43.6414, 38.8729),
  longitude = c(-112.0667, -84.4678, -76.6219, -71.0972, -87.6553, -87.6339, -84.5064, -81.6852, 
                -104.9942, -83.0485, -95.3550, -94.4803, -117.8827, -118.2400, -80.2195, -87.9711, 
                -93.2777, -73.8458, -73.9262, -122.2005, -75.1665, -80.0057, -117.1573, -122.3316, 
                -122.3890, -90.1929, -82.6534, -97.0823, -79.3891, -77.0074)
)

# Loop over the stadiums to get weather data for each
for (i in 1:nrow(stadiums)) {
  team_abbr <- stadiums$team[i]
  lat <- stadiums$latitude[i]
  lon <- stadiums$longitude[i]
  
  assign(
    paste0(team_abbr, "_weather"),
    GET(paste0("https://api.openweathermap.org/data/2.5/data?lat=", lat, "&lon=", lon, "&units=imperial&appid=", api_key)) %>%
      content("parsed")
  )
}



# Define a function to extract temperature and conditions from each team's weather data
extract_weather_info <- function(weather_data) {
  data.frame(
    temperature = weather_data$main$temp,
    weather_condition = weather_data$weather[[1]]$description,
    humidity = weather_data$main$humidity,
    wind_speed = weather_data$wind$speed
  )
}

# Combine all the weather data into a single dataframe
stadium_weather <- bind_rows(
  ARI_weather = extract_weather_info(ARI_weather),
  ATL_weather = extract_weather_info(ATL_weather),
  BAL_weather = extract_weather_info(BAL_weather),
  BOS_weather = extract_weather_info(BOS_weather),
  CHC_weather = extract_weather_info(CHC_weather),
  CHW_weather = extract_weather_info(CHW_weather),
  CIN_weather = extract_weather_info(CIN_weather),
  CLE_weather = extract_weather_info(CLE_weather),
  COL_weather = extract_weather_info(COL_weather),
  DET_weather = extract_weather_info(DET_weather),
  HOU_weather = extract_weather_info(HOU_weather),
  KC_weather = extract_weather_info(KC_weather),
  LAA_weather = extract_weather_info(LAA_weather),
  LAD_weather = extract_weather_info(LAD_weather),
  MIA_weather = extract_weather_info(MIA_weather),
  MIL_weather = extract_weather_info(MIL_weather),
  MIN_weather = extract_weather_info(MIN_weather),
  NYM_weather = extract_weather_info(NYM_weather),
  NYY_weather = extract_weather_info(NYY_weather),
  OAK_weather = extract_weather_info(OAK_weather),
  PHI_weather = extract_weather_info(PHI_weather),
  PIT_weather = extract_weather_info(PIT_weather),
  SD_weather = extract_weather_info(SD_weather),
  SEA_weather = extract_weather_info(SEA_weather),
  SF_weather = extract_weather_info(SF_weather),
  STL_weather = extract_weather_info(STL_weather),
  TB_weather = extract_weather_info(TB_weather),
  TEX_weather = extract_weather_info(TEX_weather),
  TOR_weather = extract_weather_info(TOR_weather),
  WSH_weather = extract_weather_info(WSH_weather),
  .id = "team"
)

# View the combined dataframe
print(stadium_weather)

###

#pulling the weather data 20 mins before the game for each home city
all_data <- all_data %>%
  mutate(gameDate = ymd_hms(gameDate, tz = "UTC"))

# Convert gameDate to Pacific Time
all_data <- all_data %>%
  mutate(gameDate_Pacific = with_tz(gameDate, tzone = "America/Los_Angeles"))

# Define the current system time
current_time <- now(tzone = "America/Los_Angeles")

# Calculate the difference between game start time and current time
all_data <- all_data %>%
  mutate(time_to_game = as.numeric(difftime(gameDate_Pacific, current_time, units = "mins")))

# Filter to get games starting in 20 minutes
games_20_mins <- all_data %>%
  filter(time_to_game <= 20 & time_to_game > 0)

# Team abbreviation mapping
team_abbr_mapping <- data.frame(
  team_name = c("Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles", "Boston Red Sox",
                "Chicago Cubs", "Chicago White Sox", "Cincinnati Reds", "Cleveland Guardians", 
                "Colorado Rockies", "Detroit Tigers", "Houston Astros", "Kansas City Royals", 
                "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins", "Milwaukee Brewers", 
                "Minnesota Twins", "New York Mets", "New York Yankees", "Oakland Athletics", 
                "Philadelphia Phillies", "Pittsburgh Pirates", "San Diego Padres", "Seattle Mariners", 
                "San Francisco Giants", "St. Louis Cardinals", "Tampa Bay Rays", "Texas Rangers", 
                "Toronto Blue Jays", "Washington Nationals"),
  team_abbr = c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", 
                "HOU", "KC", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", 
                "PHI", "PIT", "SD", "SEA", "SF", "STL", "TB", "TEX", "TOR", "WSH")
)

# Merge to get team abbreviations
games_20_mins <- games_20_mins %>%
  left_join(team_abbr_mapping, by = c("teams.home.team.name" = "team_name"))

# Create a list of stadiums with their latitude and longitude
stadiums <- data.frame(
  team = c("ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET", 
           "HOU", "KC", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", 
           "PHI", "PIT", "SD", "SEA", "SF", "STL", "TB", "TEX", "TOR", "WSH"),
  latitude = c(33.4453, 33.8907, 39.2839, 42.3467, 41.9484, 41.8300, 39.0975, 
               41.4962, 39.7559, 42.3390, 29.7572, 39.0515, 33.8003, 34.0739, 
               25.7781, 43.0284, 44.9817, 40.7570, 40.8296, 37.7516, 39.9055, 
               40.4475, 32.7076, 47.5914, 37.7783, 38.6226, 27.7682, 32.7513, 
               43.6414, 38.8729),
  longitude = c(-112.0667, -84.4678, -76.6219, -71.0972, -87.6553, -87.6339, 
                -84.5064, -81.6852, -104.9942, -83.0485, -95.3550, -94.4803, 
                -117.8827, -118.2400, -80.2195, -87.9711, -93.2777, -73.8458, 
                -73.9262, -122.2005, -75.1665, -80.0057, -117.1573, -122.3316, 
                -122.3890, -90.1929, -82.6534, -97.0823, -79.3891, -77.0074)
)

# Fetch weather data for games starting in 20 minutes
weather_list <- list()
for (i in 1:nrow(games_20_mins)) {
  game <- games_20_mins[i, ]
  team_abbr <- game$team_abbr
  stadium <- stadiums %>% filter(team == team_abbr)
  
  if (nrow(stadium) > 0) {
    lat <- stadium$latitude
    lon <- stadium$longitude
    
    weather_response <- GET(paste0("https://api.openweathermap.org/data/2.5/weather?lat=", lat, "&lon=", lon, "&units=imperial&appid=", api_key))
    weather_data <- content(weather_response, "parsed")
    
    weather_list[[i]] <- data.frame(
      temperature = weather_data$main$temp,
      weather_condition = weather_data$weather[[1]]$description,
      humidity = weather_data$main$humidity,
      wind_speed = weather_data$wind$speed
    )
  }
}

# Combine weather data into a single data frame
weather_data_combined <- do.call(rbind, weather_list)

# Join weather data back to the games_20_mins data frame
games_20_mins <- cbind(games_20_mins, weather_data_combined)

# Rename columns as required
games_20_mins <- games_20_mins %>%
  rename(temperature = temperature, other_weather = weather_condition)

# Print the final data frame with weather information
print(games_20_mins)
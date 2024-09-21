library(tidyverse)
library(ggplot2)
library(baseballr)
library(data.table)
library(zoo)

# Load RDS
starters <- readRDS("pitcher_game_logs.rds")
lg_avgs_p <- readRDS('league_starting_pitching_averages.rds')
team_yrly_avg <- readRDS('team_batting_by_season.rds')
starter_yrly_avg <- readRDS("player_pitching_stats_by_season.rds")

# Create new df that gets rid of unnecessary columns
starters <- starters |>
  select(1:15, 21:36, 39:41, 69:72)

starters <- starters %>%
  group_by(PlayerName, season) %>%
  arrange(Date) %>%
  # Create a row with just the specified columns and others set to NA
  do({
    first_row <- .
    empty_row <- first_row %>%
      slice(1) %>%
      mutate(
        Date = as.Date(first_row$Date[1], format = "%Y-%m-%d") - 1,
        Date = as.character(Date),
        across(-c(PlayerName, playerid, Date, Opp, teamid, season, Team, HomeAway, Age), ~ NA)
      )
    bind_rows(empty_row, first_row)
  }) %>%
  ungroup()

#need to alter the season value in first row of each pitcher in each season
#to be the previous season to bring over the averages properly when joining
#player or league averages

#create filtered df
prev_szn <- starters |>
  filter(is.na(W)) |>
  mutate(season = season - 1)

#keep only the earliest rows for each player
starter_yrly_avg <- starter_yrly_avg %>%
  group_by(PlayerName) %>%
  slice(-n()) %>%
  ungroup()

#join modified df with the averages, using a full join to keep the empty seasons
#to be able to join league averages
prev_szn<- left_join(prev_szn, starter_yrly_avg, by = c('PlayerName',
                                                        'playerid',
                                                        'season'
                                                        ))

#now league averages
prev_szn <- left_join(prev_szn, lg_avgs_p, by = 'season')

#mutating necessary columns to replace them with their per game averages
#or season long cumulative averages if that season for the player is available
prev_szn <- prev_szn |>
  mutate(H = c_H_game,
         ER = c_ER_game,
         HR = c_HR_game,
         BB = c_BB_game,
         SO = c_K_game,
         IP = c_IP_game,
         HBP = c_HBP/cumulative_G,
         IBB = c_IBB/cumulative_G,
         `H/9` = c_H9,
         `BB/9` = c_BB9, 
         `K/BB` = c_KBB, 
         `K/9` = c_K9,
         Pitches = c_p_game,
         FB = c_FB_game,
         GB = c_GB_game,
         LD = c_LD_game,
         Balls = c_Balls_game,
         R = cumulative_R
         )


#filling in the remainder of missing seasons
prev_szn <- prev_szn |>
  mutate(H = ifelse(is.na(H), lg_H_game, H),
         ER = ifelse(is.na(ER), (lg_ER/lg_G), ER),
         HR = ifelse(is.na(HR), lg_HR_game, HR),
         BB = ifelse(is.na(BB), lg_BB_game, BB),
         SO = ifelse(is.na(SO), lg_K_game, SO),
         IP = ifelse(is.na(IP), lg_IP_game, IP),
         HBP = ifelse(is.na(HBP), lg_HBP/lg_G, HBP),
         IBB = ifelse(is.na(IBB), lg_IBB/lg_G, IBB),
         `H/9` = ifelse(is.na(`H/9`), lg_H9, `H/9`),
         `BB/9` = ifelse(is.na(`BB/9`), lg_BB9, `BB/9`), 
         `K/BB` = ifelse(is.na(`K/BB`), lg_KBB, `K/BB`), 
         `K/9` = ifelse(is.na(`K/9`), lg_K9, `K/9`),
         Pitches = ifelse(is.na(Pitches), lg_P_game, Pitches),
         FB = ifelse(is.na(FB), lg_FB_game, FB),
         GB = ifelse(is.na(GB), lg_GB_game, GB),
         LD = ifelse(is.na(LD), lg_LD_game, LD),
         Balls = ifelse(is.na(Balls), lg_Balls_game, Balls),
         R = ifelse(is.na(R), lg_R/lg_G, R)
  )
#i did not collect data for the 2016 season, so those will reamin blank
#for pitchers who made their first appearance later in the season (post-June)
#I will fill in their pre-first appearance data with that season's average

#only 2016 (actual 2017) has applicable conditions
prev_szn2 <- prev_szn |>
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) |>
  filter(is.na(H) & season == 2016 & Date > as.Date('2017-06-30')) |>
  mutate(season = season + 1)

prev_szn2 <- left_join(prev_szn2, lg_avgs_p, by = 'season')

prev_szn2 <- prev_szn2 |>
  mutate(pH = lg_H_game.y,
         pER = (lg_ER.y/lg_G.y),
         pHR = lg_HR_game.y,
         pBB = lg_BB_game.y,
         pIBB = lg_IBB.y,
         pHBP = lg_HBP.y,
         pSO = lg_K_game.y,
         pKBB = lg_KBB.y,
         pK9 = lg_K9.y,
         pBB9 = lg_BB9.y,
         pH9 = lg_H9.y,
         Pitches = lg_P_game.y,
         season = season - 1,
         Date = as.character(Date))

prev_szn2 <- prev_szn2 |>
  select(1:6, 153:163)

#join the corrected rows back into primary df
prev_szn <- left_join(prev_szn, prev_szn2, by = c('PlayerName', 
                                              'playerid',
                                              'Date',
                                              'Opp',
                                              'teamid',
                                              'season'))

prev_szn <- prev_szn |>
  mutate(H = ifelse(is.na(H), pH, H),
         ER = ifelse(is.na(ER), pER, ER),
         HR = ifelse(is.na(HR), pHR, HR),
         BB = ifelse(is.na(BB), pBB, BB),
         SO = ifelse(is.na(SO), pSO, SO),
         IBB = ifelse(is.na(IBB), pIBB, IBB),
         HBP = ifelse(is.na(HBP), pHBP, HBP),
         `K/9` = ifelse(is.na(`K/9`), pK9, `K/9`),
         `BB/9` = ifelse(is.na(`BB/9`), pBB9, `BB/9`),
         `H/9` = ifelse(is.na(`H/9`), pH9, `H/9`),
         `K/BB` = ifelse(is.na(`K/BB`), pKBB, `K/BB`),
         c_p_game = ifelse(is.na(Pitches), Pitches, c_p_game),)

#select necessary columns 
prev_szn <- prev_szn |>
  select()

#selecting necessary columns and modifying names to prepare for calculations
starters <- starters |>
  select(1:38, 65:70)



# Create cumulative_ERA column (c_ERA) and running ERA that includes current and previous 5 games
starters <- starters %>%
  arrange(PlayerName, season, Date) %>%  # Ensure the data is ordered by player, season, and date
  mutate(
    # Convert IP to outs (whole number part * 3 + decimal part)
    outs_pitched = ((floor(IP) * 3) + ((IP - floor(IP)) * 10)),
    out_fraction = (IP - floor(IP)) / .3,
    
    # Convert back to innings
    IP_corrected = (floor(IP) + out_fraction)
  ) %>%
  
  # Group by player and season to calculate cumulative stats for previous games only
  group_by(PlayerName, season) %>%
  mutate(
    # Apply lag only when there are previous games
    cumulative_outs = if_else(row_number() == 1, NA_real_, lag(cumsum(outs_pitched))),
    cumulative_ER = if_else(row_number() == 1, NA_real_, lag(cumsum(ER))),
    
    # Cumulative innings pitched
    cumulative_IP = cumulative_outs / 3,
    
    # Calculate cumulative ERA for previous games
    c_ERA = ifelse(is.na(cumulative_IP) | cumulative_IP == 0, NA, (cumulative_ER * 9) / cumulative_IP)
  ) %>%
  
  ungroup()  # Ungroup when done

# Create rolling 6-game ERA
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_ERA = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative ERA
        c_ERA[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
        rolling_cumulative_ER <- sum(data$ER)
        rolling_cumulative_IP <- sum(data$IP_corrected)
        
        if (rolling_cumulative_IP == 0) {
          NA
        } else {
          (rolling_cumulative_ER * 9) / rolling_cumulative_IP
        }
      }
    })
  ) %>%
  ungroup()

# Create cumulative K/9
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    # Cumulative strikeouts per player
    cumulative_SO = cumsum(SO),
    
    # Calculate cumulative K/9, but handle cases where IP is zero to avoid division by zero
    c_K9 = ifelse(cumulative_IP == 0, NA, (cumulative_SO * 9) / cumulative_IP)
  ) %>%
  
  ungroup()  # Ungroup when done

# Create 6-game rolling K/9
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_K9 = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative K/9
        c_K9[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-5):i, ]
        rolling_cumulative_SO <- sum(data$SO)
        rolling_cumulative_IP <- sum(data$IP_corrected)
        
        if (rolling_cumulative_IP == 0) {
          NA
        } else {
          (rolling_cumulative_SO * 9) / rolling_cumulative_IP
        }
      }
    })
  ) %>%
  ungroup()

# Create cumulative QS Ratio
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    # Cumulative QS and GS per player
    cumulative_QS = cumsum(QS),
    cumulative_GS = cumsum(GS),
    
    # Calculate cumulative QSR (QS ratio), but handle cases where IP is zero to avoid division by zero
    c_QSR = ifelse(cumulative_IP == 0, NA, (cumulative_QS) / cumulative_GS)
  ) %>%
  
  ungroup()  # Ungroup when done

# Create cumulative BB/9
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    # Cumulative walks per player
    cumulative_BB = cumsum(BB),
    
    # Calculate cumulative BB/9, but handle cases where IP is zero to avoid division by zero
    c_BB9 = ifelse(cumulative_IP == 0, NA, (cumulative_BB * 9) / cumulative_IP)
  ) %>%
  
  ungroup()  # Ungroup when done

# Create 6-game rolling BB/9
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_BB9 = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative BB/9
        c_BB9[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-5):i, ]
        rolling_cumulative_BB <- sum(data$BB)
        rolling_cumulative_IP <- sum(data$IP_corrected)
        
        if (rolling_cumulative_IP == 0) {
          NA
        } else {
          (rolling_cumulative_BB * 9) / rolling_cumulative_IP
        }
      }
    })
  ) %>%
  ungroup()

# Create rolling H/9
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    # Cumulative hits per player
    cumulative_H = cumsum(H),
    
    # Calculate cumulative H/9, but handle cases where IP is zero to avoid division by zero
    c_H9 = ifelse(cumulative_IP == 0, NA, (cumulative_H * 9) / cumulative_IP)
  ) %>%
  
  ungroup()  # Ungroup when done

# Create 6-game rolling H/9
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_H9 = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative H/9
        c_H9[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-5):i, ]
        rolling_cumulative_H <- sum(data$H)
        rolling_cumulative_IP <- sum(data$IP_corrected)
        
        if (rolling_cumulative_IP == 0) {
          NA
        } else {
          (rolling_cumulative_H * 9) / rolling_cumulative_IP
        }
      }
    })
  ) %>%
  ungroup()

# Calculate cumulative K/BB
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    # Calculate cumulative K/BB, but handle cases where cumulative_BB is zero to avoid division by zero
    c_KBB = ifelse(cumulative_BB == 0, NA, cumulative_SO / cumulative_BB)
  ) %>%
  ungroup()

# Calculate rolling K/BB (last 6 games)
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_KBB = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative K/BB
        c_KBB[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-5):i, ]
        rolling_cumulative_SO <- sum(data$SO)
        rolling_cumulative_BB <- sum(data$BB)
        
        if (rolling_cumulative_BB == 0) {
          NA  # Avoid division by zero
        } else {
          rolling_cumulative_SO / rolling_cumulative_BB
        }
      }
    })
  ) %>%
  ungroup()

# Calculate cumulative HR/9
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    # Cumulative HR and HR/9, handle cases where cumulative_IP is zero to avoid division by zero
    cumulative_HR = cumsum(HR),
    c_HR9 = ifelse(cumulative_IP == 0, NA, (cumulative_HR * 9) / cumulative_IP)
  ) %>%
  ungroup()

# Calculate rolling HR/9 (last 6 games)
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_HR9 = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative HR/9
        c_HR9[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-5):i, ]
        rolling_cumulative_HR <- sum(data$HR)
        rolling_cumulative_IP <- sum(data$IP_corrected)
        
        if (rolling_cumulative_IP == 0) {
          NA
        } else {
          (rolling_cumulative_HR * 9) / rolling_cumulative_IP
        }
      }
    })
  ) %>%
  ungroup()

# Calculate rolling ERA for all games (last 10 games)
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_ERA_10 = sapply(seq_len(n()), function(i) {
      if (i <= 10) {
        # For rows with 10 or fewer previous games, use cumulative ERA
        c_ERA[i]
      } else {
        # Manually subset the previous 10 games including the current one
        data <- cur_data()[(i-9):i, ]
        rolling_cumulative_ER <- sum(data$ER)
        rolling_cumulative_IP <- sum(data$IP_corrected)
        
        if (rolling_cumulative_IP == 0) {
          NA
        } else {
          (rolling_cumulative_ER * 9) / rolling_cumulative_IP
        }
      }
    })
  ) %>%
  ungroup()

# Calculate cumulative WHIP
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    # Calculate WHIP (Walks + Hits) / IP
    c_WHIP = ifelse(cumulative_IP == 0, NA, (cumulative_BB + cumulative_H) / cumulative_IP)
  ) %>%
  ungroup()

# Calculate rolling WHIP (last 6 games)
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_WHIP = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative WHIP
        c_WHIP[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-5):i, ]
        rolling_cumulative_BB <- sum(data$BB)
        rolling_cumulative_H <- sum(data$H)
        rolling_cumulative_IP <- sum(data$IP_corrected)
        
        if (rolling_cumulative_IP == 0) {
          NA
        } else {
          (rolling_cumulative_BB + rolling_cumulative_H) / rolling_cumulative_IP
        }
      }
    })
  ) %>%
  ungroup()












saveRDS(team_rolling, "team_rolling_game_averages.rds")

saveRDS(starters, "all_pitcher_game_by_game_averages.rds")
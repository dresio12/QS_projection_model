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
team_gbg <- readRDS('team_batting_by_game.rds')


# Create new df that gets rid of unnecessary columns and only has data for
#starting pitching 

starters <- starters |>
  select(1:15, 21:36, 39:41, 69:72) |>
  filter(GS == '1')

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

#join modified df with the averages

#some data wont exist - that's ok because i dont have previous season data for
#every pitcher
prev_szn<- left_join(prev_szn, starter_yrly_avg, by = c('PlayerName',
                                                        'playerid',
                                                        'season'
                                                        ))

#join league averages to eventually fill in empty rows
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
         R = cumulative_R,
         G = cumulative_G,
         GS = cumulative_GS,
         QS = cumulative_QS,
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
         R = ifelse(is.na(R), lg_R/lg_G, R),
         G = ifelse(is.na(G), lg_G, G),
         GS = ifelse(is.na(GS), lg_GS, GS),
         QS = ifelse(is.na(QS), lg_QS, QS)
  )
#i did not collect data for the 2016 season, so those will remain blank
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
         pER = lg_ER.y/lg_G.y,
         pHR = lg_HR_game.y,
         pBB = lg_BB_game.y,
         pSO = lg_K_game.y,
         pIP = lg_IP_game.y,
         pHBP = lg_HBP.y/lg_G.y,
         pIBB = lg_IBB.y/lg_G.y,
         `pH/9` = lg_H9.y,
         `pBB/9` = lg_BB9.y, 
         `pK/BB` = lg_KBB.y, 
         `pK/9` = lg_K9.y,
         pPitches = lg_P_game.y,
         pFB = lg_FB_game.y,
         pGB = lg_GB_game.y,
         pLD = lg_LD_game.y,
         pR = lg_R.y/lg_G.y,
         pGS = lg_GS.y,
         pQS = lg_QS.y,
         Date = as.character(Date))

prev_szn2 <- prev_szn2 |>
  select(1:6, 169:187)

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
         IP = ifelse(is.na(IP), pIP, IP),
         IBB = ifelse(is.na(IBB), pIBB, IBB),
         HBP = ifelse(is.na(HBP), pHBP, HBP),
         `K/9` = ifelse(is.na(`K/9`), `pK/9`, `K/9`),
         `BB/9` = ifelse(is.na(`BB/9`), `pBB/9`, `BB/9`),
         `H/9` = ifelse(is.na(`H/9`), `pH/9`, `H/9`),
         `K/BB` = ifelse(is.na(`K/BB`), `pK/BB`, `K/BB`),
         Pitches = ifelse(is.na(Pitches), pPitches, Pitches),
         FB = ifelse(is.na(FB), pFB, FB),
         GB = ifelse(is.na(GB), pGB, GB),
         LD = ifelse(is.na(LD), pLD, LD),
         GS = ifelse(is.na(GS), pGS, GS),
         QS = ifelse(is.na(QS), pQS, QS),
         R = ifelse(is.na(R), pR, R))

#select necessary columns 
prev_szn <- prev_szn |>
  select(1:37) |>
  rename(Age = Age.x,
         Team = Team.x)

#remove NA rows since 2016 data isn't in DF
clean_prev <- prev_szn |>
  filter(!is.na(IP))

#join back to starters DF to now include necessary rows for start 1
starters <- starters |>
  select(1:9, 12:24, 27:34, 37)

clean_prev <- clean_prev |>
  select(1:9, 12:24, 27:34, 37) |>
  mutate(season = season + 1)

#joining dfs
starters <- left_join(starters, clean_prev, by = c('PlayerName', 'playerid', 'Date',
                                               'Opp', 'teamid', 'season', 'Team',
                                               'HomeAway', 'Age'))

starters <- starters %>%
  mutate(
    ERA = ifelse(is.na(ERA.x), ERA.y, ERA.x),
    G = ifelse(is.na(G.x), G.y, G.x),
    GS = ifelse(is.na(GS.x), GS.y, GS.x),
    QS = ifelse(is.na(QS.x), QS.y, QS.x),
    IP = ifelse(is.na(IP.x), IP.y, IP.x),
    TBF = ifelse(is.na(TBF.x), TBF.y, TBF.x),
    H = ifelse(is.na(H.x), H.y, H.x),
    R = ifelse(is.na(R.x), R.y, R.x),
    ER = ifelse(is.na(ER.x), ER.y, ER.x),
    HR = ifelse(is.na(HR.x), HR.y, HR.x),
    BB = ifelse(is.na(BB.x), BB.y, BB.x),
    IBB = ifelse(is.na(IBB.x), IBB.y, IBB.x),
    HBP = ifelse(is.na(HBP.x), HBP.y, HBP.x),
    SO = ifelse(is.na(SO.x), SO.y, SO.x),
    `K/9` = ifelse(is.na(`K/9.x`), `K/9.y`, `K/9.x`),
    `BB/9` = ifelse(is.na(`BB/9.x`), `BB/9.y`, `BB/9.x`),
    `H/9` = ifelse(is.na(`H/9.x`), `H/9.y`, `H/9.x`),
    `K/BB` = ifelse(is.na(`K/BB.x`), `K/BB.y`, `K/BB.x`),
    GB = ifelse(is.na(GB.x), GB.y, GB.x),
    FB = ifelse(is.na(FB.x), FB.y, FB.x),
    LD = ifelse(is.na(LD.x), LD.y, LD.x),
    Pitches = ifelse(is.na(Pitches.x), Pitches.y, Pitches.x)
  )

starters <- starters %>%
  select(-ends_with(".x"), -ends_with(".y"))

#remove any empty rows because i dont have 2016 data
clean_starters <- starters |>
  filter(!is.na(IP))

#save as new dataframe
saveRDS(clean_starters, "pitcher_game_logs_with_predates.rds")
clean_starters <- readRDS("pitcher_game_logs_with_predates.rds")

#cumulative ERA average
clean_starters <- clean_starters %>%
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
    # Apply lag for cumulative outs and ER
    cumulative_outs = lag(cumsum(outs_pitched), default = 0),
    cumulative_ER = lag(cumsum(ER), default = 0),
    
    # Cumulative innings pitched
    cumulative_IP = cumulative_outs / 3,
    
    # Calculate cumulative ERA for previous games
    c_ERA = if_else(cumulative_IP == 0, NA_real_, (cumulative_ER * 9) / cumulative_IP)
  ) %>%
  
  ungroup()  # Ungroup when done


# Create rolling 6-game ERA
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_ERA = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative ERA
        c_ERA[i]
      } else {
        # Manually subset the previous 6 games
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
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    # Cumulative strikeouts per player
    cumulative_SO = lag(cumsum(SO), default = 0),
    
    # Calculate cumulative K/IP, but handle cases where IP is zero to avoid division by zero
    c_K9 = ifelse(cumulative_IP == 0, NA_real_, (cumulative_SO * 9) / cumulative_IP)
  ) %>%
  
  ungroup()  # Ungroup when done

# Create 6-game rolling K/9
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_K9 = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative K/IP
        c_K9[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
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
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    # Cumulative QS and GS per player
    cumulative_QS = lag(cumsum(QS), default = 0),
    cumulative_GS = lag(cumsum(GS), default = 0),
    
    
    # Calculate cumulative QSR (QS ratio), but handle cases where IP is zero to avoid division by zero
    c_QSR = ifelse(cumulative_IP == 0, NA_real_, cumulative_QS / cumulative_GS)
  ) %>%
  
  ungroup()  # Ungroup when done

#Create rolling QS_ratio
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_QSR = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative BB/IP
        c_QSR[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
        rolling_cumulative_QS <- sum(data$QS)
        rolling_cumulative_GS <- sum(data$GS)
        
        if (rolling_cumulative_GS == 0) {
          NA
        } else {
          (rolling_cumulative_QS) / rolling_cumulative_GS
        }
      }
    })
  ) %>%
  ungroup()


#create cumulative BB/9
clean_starters <- clean_starters %>%
  # Group by player and season to calculate cumulative stats
  group_by(PlayerName, season) %>%
  mutate(
    # Cumulative strikeouts per player
    cumulative_BB = lag(cumsum(BB), default = 0),
    
    # Calculate cumulative BB/IP, but handle cases where IP is zero to avoid division by zero
    c_BB9 = ifelse(cumulative_IP == 0, NA, (cumulative_BB * 9) / cumulative_IP)
  ) %>%
  
  ungroup()  # Ungroup when done

#create 6-game rolling BB/9
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_BB9 = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative BB/IP
        c_BB9[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
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



#create cumulative H/9
clean_starters <- clean_starters %>%
  # Group by player and season to calculate cumulative stats
  group_by(PlayerName, season) %>%
  mutate(
    # Cumulative strikeouts per player
    cumulative_H = lag(cumsum(H), default = 0),
    
    # Calculate cumulative H/IP, but handle cases where IP is zero to avoid division by zero
    c_H9 = ifelse(cumulative_IP == 0, NA, (cumulative_H * 9) / cumulative_IP)
  ) %>%
  
  ungroup()  # Ungroup when done

#create 6-game rolling H/9
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_H9 = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative H/IP
        c_H9[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
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
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    # Calculate cumulative K/BB, but handle cases where cumulative_BB is zero to avoid division by zero
    c_KBB = ifelse(cumulative_BB == 0, NA, cumulative_SO / cumulative_BB)
  ) %>%
  ungroup()

# Calculate rolling K/BB (last 6 games)
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_KBB = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative K/BB
        c_KBB[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1):i, ]
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
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    # Cumulative HR and HR/IP, handle cases where cumulative_IP is zero to avoid division by zero
    cumulative_HR = lag(cumsum(HR), default = 0),
    c_HR9 = ifelse(cumulative_IP == 0, NA, (cumulative_HR * 9) / cumulative_IP)
  ) %>%
  ungroup()

# Calculate rolling HR/IP (last 6 games)
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_HR9 = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative HR/IP
        c_HR9[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
        rolling_cumulative_HR <- sum(data$HR)
        rolling_cumulative_IP <- sum(data$IP_corrected)
        
        if (rolling_cumulative_IP == 0) {
          NA  # Avoid division by zero
        } else {
          (rolling_cumulative_HR * 9) / rolling_cumulative_IP
        }
      }
    })
  ) %>%
  ungroup()


# Calculate cumulative RA9
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    # Cumulative runs allowed and RA9, handle cases where cumulative_IP is zero to avoid division by zero
    cumulative_R = lag(cumsum(R), default = 0),
    c_RA9 = ifelse(cumulative_IP == 0, NA, (cumulative_R * 9) / cumulative_IP)
  ) %>%
  ungroup()

# Calculate rolling RA9 (last 6 games)
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_RA9 = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative RA9
        c_RA9[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
        rolling_cumulative_R <- sum(data$R)
        rolling_cumulative_IP <- sum(data$IP_corrected)
        
        if (rolling_cumulative_IP == 0) {
          NA  # Avoid division by zero
        } else {
          (rolling_cumulative_R * 9) / rolling_cumulative_IP
        }
      }
    })
  ) %>%
  ungroup()



#Calculating cumulative and rolling FIP
# Calculate league-wide statistics, excluding zero IP cases to avoid division by zero
league_averages <- clean_starters %>%
  filter(cumulative_IP > 0) %>%  # Exclude rows where cumulative_IP is zero
  group_by(season) %>%
  summarize(
    lgERA = mean(cumulative_ER * 9 / cumulative_IP, na.rm = TRUE),
    lgHR = mean(HR, na.rm = TRUE),
    lgBB = mean(BB, na.rm = TRUE),
    lgHBP = mean(HBP, na.rm = TRUE),
    lgK = mean(SO, na.rm = TRUE),
    lgIP = mean(IP_corrected, na.rm = TRUE)
  )

# Calculate FIP constant for each season
fip_constants <- league_averages %>%
  mutate(
    FIP_constant = lgERA - (((13 * lgHR) + (3 * (lgBB + lgHBP)) - (2 * lgK)) / lgIP)
  )

# Merge with clean_starters data to get FIP constants
clean_starters <- clean_starters %>%
  left_join(fip_constants, by = "season") 

#calculative cumulative FIP
clean_starters <- clean_starters |>
  group_by(PlayerName, season) %>%
  mutate(
    cumulative_HBP = lag(cumsum(HBP), default = 0),
    # Calculate FIP
    FIP = ((13 * cumulative_HR) + (3 * (cumulative_BB + cumulative_HBP)) - (2 * cumulative_SO)) / cumulative_IP + FIP_constant
  )


# Rolling FIP (previous 6 games including the current one)
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_FIP = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative FIP
        FIP[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
        rolling_cumulative_HR <- sum(data$HR)
        rolling_cumulative_BB <- sum(data$BB)
        rolling_cumulative_HBP <- sum(data$HBP)
        rolling_cumulative_SO <- sum(data$SO)
        rolling_cumulative_IP <- sum(data$IP_corrected)
        
        if (rolling_cumulative_IP == 0) {
          NA
        } else {
          rolling_FIP_constant <- mean(data$FIP_constant)
          ((13 * rolling_cumulative_HR) + (3 * (rolling_cumulative_BB + rolling_cumulative_HBP)) - (2 * rolling_cumulative_SO)) / rolling_cumulative_IP + rolling_FIP_constant
        }
      }
    })
  ) %>%
  ungroup()


# Calculate cumulative WHIP
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    # Cumulative WHIP
    cumulative_WHIP = ifelse(cumulative_IP == 0, NA, (cumulative_BB + cumulative_H) / cumulative_IP)
  )


# Calculate rolling WHIP
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_WHIP = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative WHIP
        cumulative_WHIP[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
        rolling_cumulative_walks <- sum(data$BB)
        rolling_cumulative_hits <- sum(data$H)
        rolling_cumulative_IP <- sum(data$IP_corrected)
        
        if (rolling_cumulative_IP == 0) {
          NA
        } else {
          (rolling_cumulative_walks + rolling_cumulative_hits) / rolling_cumulative_IP
        }
      }
    })
  ) %>%
  ungroup()



# Calculate cumulative and rolling FB% and GB%
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    cumulative_FB = lag(cumsum(FB), default = 0),
    cumulative_GB = lag(cumsum(GB), default = 0),
    cumulative_LD = lag(cumsum(LD), default = 0),
    cumulative_Balls = cumulative_FB + cumulative_GB + cumulative_LD,
    
    # Cumulative Flyball% (FB%) and Groundball% (GB%)
    c_FB_pct = ifelse(cumulative_Balls == 0, NA, cumulative_FB / cumulative_Balls),
    c_GB_pct = ifelse(cumulative_Balls == 0, NA, cumulative_GB / cumulative_Balls)
  ) %>%
  
  # Calculate rolling FB% and GB% using the previous 6 games including the current one
  mutate(
    rolling_FB_pct = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative FB%
        c_FB_pct[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
        rolling_cumulative_FB <- sum(data$FB)
        rolling_cumulative_GB <- sum(data$GB)
        rolling_cumulative_LD <- sum(data$LD)
        rolling_cumulative_Balls <- rolling_cumulative_FB + rolling_cumulative_GB + rolling_cumulative_LD
        
        if (rolling_cumulative_Balls == 0) {
          NA
        } else {
          rolling_cumulative_FB / rolling_cumulative_Balls
        }
      }
    }),
    
    rolling_GB_pct = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative GB%
        c_GB_pct[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
        rolling_cumulative_FB <- sum(data$FB)
        rolling_cumulative_GB <- sum(data$GB)
        rolling_cumulative_LD <- sum(data$LD)
        rolling_cumulative_Balls <- rolling_cumulative_FB + rolling_cumulative_GB + rolling_cumulative_LD
        
        if (rolling_cumulative_Balls == 0) {
          NA
        } else {
          rolling_cumulative_GB / rolling_cumulative_Balls
        }
      }
    })
  ) %>%
  ungroup()


#calculate cumulative and rolling K/game, BB/game, ER/game, H/game, HR/game, IP/game
clean_starters <- clean_starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    cumulative_G = cumsum(G),
    c_K_game = ifelse(cumulative_G == 0, NA, cumulative_SO / cumulative_G),
    c_BB_game = ifelse(cumulative_G == 0, NA, cumulative_BB / cumulative_G),
    c_ER_game = ifelse(cumulative_G == 0, NA, cumulative_ER / cumulative_G),
    c_IP_game = ifelse(cumulative_IP == 0, 0, cumulative_IP / cumulative_G),
    c_HR_game = ifelse(cumulative_G == 0, NA, cumulative_HR / cumulative_G),
    c_H_game = ifelse(cumulative_G == 0, NA, cumulative_H / cumulative_G),
    c_FB_game = ifelse(cumulative_G == 0, NA, cumulative_FB / cumulative_G),
    c_GB_game = ifelse(cumulative_G == 0, NA, cumulative_GB / cumulative_G),
    c_LD_game = ifelse(cumulative_G == 0, NA, cumulative_LD / cumulative_G),
    c_Balls_game = ifelse(cumulative_G == 0, NA, cumulative_Balls / cumulative_G),
    
    # Rolling Stats (last 6 games)
    rolling_K_game = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative GB%
        c_K_game[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
        rolling_cumulative_K <- sum(data$SO)
        rolling_cumulative_G <- sum(data$G)
        
        if (rolling_cumulative_G == 0) {
          NA
        } else {
          rolling_cumulative_K / rolling_cumulative_G
        }
      }
    }),
    
    rolling_BB_game = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative GB%
        c_BB_game[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
        rolling_cumulative_BB <- sum(data$BB)
        rolling_cumulative_G <- sum(data$G)
        
        if (rolling_cumulative_G == 0) {
          NA
        } else {
          rolling_cumulative_BB / rolling_cumulative_G
        }
      }
    }),
    
    rolling_ER_game = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative GB%
        c_ER_game[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
        rolling_cumulative_ER <- sum(data$ER)
        rolling_cumulative_G <- sum(data$G)
        
        if (rolling_cumulative_G == 0) {
          NA
        } else {
          rolling_cumulative_ER / rolling_cumulative_G
        }
      }
    }),
    
    rolling_H_game = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative GB%
        c_H_game[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
        rolling_cumulative_H <- sum(data$H)
        rolling_cumulative_G <- sum(data$G)
        
        if (rolling_cumulative_G == 0) {
          NA
        } else {
          rolling_cumulative_H / rolling_cumulative_G
        }
      }
    }),
    
    rolling_HR_game = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative GB%
        c_HR_game[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
        rolling_cumulative_HR <- sum(data$HR)
        rolling_cumulative_G <- sum(data$G)
        
        if (rolling_cumulative_G == 0) {
          NA
        } else {
          rolling_cumulative_HR / rolling_cumulative_G
        }
      }
    }),
    
    rolling_IP_game = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative GB%
        c_IP_game[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
        rolling_cumulative_IP <- sum(data$IP)
        rolling_cumulative_G <- sum(data$G)
        
        if (rolling_cumulative_IP == 0) {
          0
        } else {
          rolling_cumulative_IP / rolling_cumulative_G
        }
      }
    })
  ) %>%
  ungroup()

# Calculate cumulative pitches
clean_starters <- clean_starters %>%
  arrange(PlayerName, season, Date) %>%  # Ensure data is ordered by player, season, and date
  group_by(PlayerName, season) %>%
  mutate(
    # Cumulative pitches/G
    cumulative_pitches = cumsum(Pitches),
    c_p_game = ifelse(cumulative_G == 0, NA, (cumulative_pitches) / cumulative_G)
  ) %>%
  ungroup()

# Calculate rolling P/game (last 6 games)
clean_starters <- clean_starters %>%
  arrange(PlayerName, season, Date) %>%  # Ensure data is ordered by player, season, and date
  group_by(PlayerName, season) %>%
  mutate(
    rolling_p_game = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative RA9
        c_p_game[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-6):(i-1), ]
        rolling_cumulative_pitches <- sum(data$Pitches)
        rolling_cumulative_G <- sum(data$G)
        
        if (rolling_cumulative_G == 0) {
          NA  # Avoid division by zero
        } else {
          (rolling_cumulative_pitches) / rolling_cumulative_G
        }
      }
    })
  ) %>%
  ungroup()

#
#
#
#joining team game by game averages and pitching averages to create 
#final statistical df

#first need to remove @ sign from DF
clean_starters$Opp <- gsub("@", "", clean_starters$Opp)

#change to date format
clean_starters <- clean_starters %>%
  mutate(Date = as.Date(Date))

#join dfs
all_data <- left_join(clean_starters, team_gbg, by = c('PlayerName' = 'T_PlayerName',
                                                       'season' = 'T_season',
                                                       'Date' = 'T_Date',
                                                       'Team' = 'T_Team',
                                                       'Opp' = 'T_team_name',
                                                       'HomeAway' = 'T_HomeAway'
                                                       ))

#remove empty pre-season rows since everything ahs been lag-calculated
all_data <- all_data |>
  select(-170, -171) |>
  filter(!is.na(ERA))



#first row of each pitcher for team variables

#need to get each team's first game of the season
team_prev <- all_data |>
  mutate(Date = as.Date(Date))

team_prev <- team_prev |>
  group_by(season, Opp) |>
  slice_min(order_by = Date)

#reduce season by 1 to use previous szn data
team_prev <- team_prev |>
  mutate(season = season - 1)

#need to add both park factors for each team to each row, then select the proper 
#park factor column
team_prev_with_opp_park <- team_prev %>%
  left_join(team_yrly_avg, by = c('season' = 'T_season',
                                  "Opp" = "T_team_name")) %>%  # Replace 'team_column' with the correct column in team_yrly_avg
  rename(Opp_T_3yr = T_3yr, Opp_T_1yr = T_1yr)

#create small df for simplicity
pf_sample <- team_yrly_avg |>
  select(T_season, T_team_name, T_3yr, T_1yr)

# Join again to get park factors for the pitcher's team
team_prev_with_both_park <- team_prev_with_opp_park %>%
  left_join(pf_sample, by = c('season' = 'T_season',
                              "Team" = "T_team_name")) %>%  # Replace 'team_column' with the correct column in team_yrly_avg
  rename(P_T_3yr = T_3yr, P_T_1yr = T_1yr)

# Use conditional logic to choose the home team's park factors if HomeAway is "H"
team_prev <- team_prev_with_both_park %>%
  mutate(
    T_3yr = if_else(HomeAway == "H", P_T_3yr, Opp_T_3yr),
    T_1yr = if_else(HomeAway == "H", P_T_1yr, Opp_T_1yr)
  )

#remove extra columns and change back to correct season
team_prev <- team_prev |>
  select(1:8, 214, 215) |>
  mutate(season = season + 1)

#join to add proper park factors, all done
all_data <- left_join(all_data, team_prev)



saveRDS(all_data, 'all_pitching_data.rds')

saveRDS(clean_starters, "all_pitcher_game_by_game_averages.rds")

library(tidyverse)
library(ggplot2)
library(baseballr)
library(data.table)
library(zoo)

# Load RDS
pitcher_stats <- readRDS("pitcher_game_logs.rds")

# Create new df that gets rid of unnecessary columns
# Initial trim
starters <- pitcher_stats |>
  select(1:75)

# Final trim
starters <- starters |>
  select(1:15, 21:36, 39:41, 69:72)

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
  
  # Group by player and season to calculate cumulative stats
  group_by(PlayerName, season) %>%
  mutate(
    # Cumulative outs and earned runs per player
    cumulative_outs = cumsum(outs_pitched),
    cumulative_ER = cumsum(ER),
    
    # Cumulative innings pitched
    cumulative_IP = cumulative_outs / 3,
    
    # Calculate cumulative ERA, but handle cases where IP is zero to avoid division by zero
    c_ERA = ifelse(cumulative_IP == 0, NA, (cumulative_ER * 9) / cumulative_IP)
  ) %>%
  
  ungroup()  # Ungroup when done

# Create rolling 6-game (previous 5 + current) ERA
starters <- starters %>%
  group_by(PlayerName, season) %>%
  mutate(
    rolling_ERA = sapply(seq_len(n()), function(i) {
      if (i <= 6) {
        # For rows with 6 or fewer previous games, use cumulative ERA
        c_ERA[i]
      } else {
        # Manually subset the previous 6 games including the current one
        data <- cur_data()[(i-5):i, ]
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


#Getting cumulative season stats for each pitcher

#stats we already have
c_stats <- starters %>%
  group_by(PlayerName, season) %>%
  slice_max(Date) %>%
  ungroup()

c_stats <- c_stats |>
  select(1, 2, 6, 7, 9, 47, 57, 60, 50:54, 39:45, 56, 59, 
         48, 61, 62, 64, 65, 73:74, 76, 78:80, 82, 83, 86:92, 99:100)

#getting total sums for stats not included above
other_c <- starters |>
  group_by(PlayerName, playerid, season) %>%
  summarize(c_W = sum(W),
            c_L = sum(L),
            c_TBF = sum(TBF),
            c_IBB = sum(IBB),
            c_HBP = sum(HBP),
            c_WP = sum(WP),
            c_WPA = sum(WPA)
            ) |>
  ungroup()


#join dfs
per_season <- left_join(c_stats, other_c)

#league averages df

#use only pitchers that started 80% of their games
#to try and collect data from true starters
per_season <- per_season |>
  mutate(start_percentage = cumulative_GS/cumulative_G)

#pulling "starters"
s_only <- per_season |>
  filter(start_percentage >= .8)

s_only <- s_only |>
  mutate(
    FIP = ifelse(is.infinite(FIP) | cumulative_IP == 0, NA, FIP)  # Replace infinite values with NA
  )

lg_avgs <- s_only |>
  group_by(season) |>
  summarize(lg_H = sum(cumulative_H * cumulative_IP) / sum(cumulative_IP),
            lg_SO = sum(cumulative_SO * cumulative_IP) / sum(cumulative_IP),
            lg_HR = sum(cumulative_HR * cumulative_IP) / sum(cumulative_IP),
            lg_R = sum(cumulative_R * cumulative_IP) / sum(cumulative_IP),
            lg_BB = sum(cumulative_BB * cumulative_IP) / sum(cumulative_IP),
            lg_OUTSPITCHED = sum(outs_pitched * cumulative_IP) / sum(cumulative_IP),
            lg_OUTS = sum(cumulative_outs * cumulative_IP) / sum(cumulative_IP),
            lg_ER = sum(cumulative_ER * cumulative_IP) / sum(cumulative_IP),
            lg_IP = sum(cumulative_IP * cumulative_IP) / sum(cumulative_IP),
            lg_ERA = sum(c_ERA * cumulative_IP) / sum(cumulative_IP),
            lg_KBB = sum(c_KBB * cumulative_IP, na.rm = TRUE) / sum(cumulative_IP, na.rm = TRUE),
            lg_HR9 = sum(c_HR9 * cumulative_IP) / sum(cumulative_IP),
            lg_K9 = sum(c_K9 * cumulative_IP) / sum(cumulative_IP),
            lg_RA9 = sum(c_RA9 * cumulative_IP) / sum(cumulative_IP),
            lg_WHIP = sum(cumulative_WHIP * cumulative_IP) / sum(cumulative_IP),
            lg_FIP = sum(FIP * cumulative_IP, na.rm = TRUE) / sum(cumulative_IP, na.rm = TRUE),
            lg_FB = sum(c_FB_pct * cumulative_IP) / sum(cumulative_IP),
            lg_GB = sum(c_GB_pct * cumulative_IP) / sum(cumulative_IP),
            lg_W = sum(c_W * cumulative_IP) / sum(cumulative_IP),
            lg_L = sum(c_L * cumulative_IP) / sum(cumulative_IP),
            lg_GS = sum(cumulative_GS * cumulative_IP) / sum(cumulative_IP),
            lg_TBF = sum(c_TBF * cumulative_IP) / sum(cumulative_IP),
            lg_IBB = sum(c_IBB * cumulative_IP) / sum(cumulative_IP),
            lg_HBP = sum(c_HBP * cumulative_IP) / sum(cumulative_IP),
            lg_WP = sum(c_WP * cumulative_IP) / sum(cumulative_IP),
            lg_G = sum(cumulative_G * cumulative_IP) / sum(cumulative_IP),
            lg_H_game = sum(c_H_game * cumulative_IP) / sum(cumulative_IP),
            lg_HR_game = sum(c_HR_game * cumulative_IP) / sum(cumulative_IP),
            lg_BB_game = sum(c_BB_game * cumulative_IP) / sum(cumulative_IP),
            lg_K_game = sum(c_K_game * cumulative_IP) / sum(cumulative_IP),
            lg_IP_game = sum(c_IP_game * cumulative_IP) / sum(cumulative_IP),
            lg_P_game = sum(c_p_game * cumulative_IP) / sum(cumulative_IP),
            lg_WPA = sum(c_WPA * cumulative_IP, na.rm = TRUE) / sum(cumulative_IP, na.rm = TRUE)
  )


####


#Begin cumulative stats for individual teams
team17 <- baseballr::fg_team_batter(startseason = 2017, endseason = 2017)
team18 <- baseballr::fg_team_batter(startseason = 2018, endseason = 2018)
team19 <- baseballr::fg_team_batter(startseason = 2019, endseason = 2019)
team20 <- baseballr::fg_team_batter(startseason = 2020, endseason = 2020)
team21 <- baseballr::fg_team_batter(startseason = 2021, endseason = 2021)
team22 <- baseballr::fg_team_batter(startseason = 2022, endseason = 2022)
team23 <- baseballr::fg_team_batter(startseason = 2023, endseason = 2023)
team24 <- baseballr::fg_team_batter(startseason = 2024, endseason = 2024)

team17 <- team17 |> select(1:75)
team18 <- team18 |> select(1:75)
team19 <- team19 |> select(1:75)
team20 <- team20 |> select(1:75)
team21 <- team21 |> select(1:75)
team22 <- team22 |> select(1:75)
team23 <- team23 |> select(1:75)
team24 <- team24 |> select(1:75)

teams <- rbind(team17, team18, team19, team20, team21, team22, team23, team24)
teams <- teams |>
  select(1:2, 5:27, 35:42, 45, 46, 48, 52:54, 64, 69, 74)

#bring in park factors
pf <- readRDS('all_park_factors.rds')
pf <- pf |>
  select(1,2,4,5)

team_mapping <- c(
  "New York Yankees" = "NYY",
  "Boston Red Sox" = "BOS",
  "Baltimore Orioles" = "BAL",
  "Toronto Blue Jays" = "TOR",
  "Tampa Bay Rays" = "TBR",
  "Oakland Athletics" = "OAK",
  "Los Angeles Angels" = "LAA",
  "Seattle Mariners" = "SEA",
  "Texas Rangers" = "TEX",
  "Houston Astros" = "HOU",
  "Washington Nationals" = "WSN",
  "New York Mets" = "NYM",
  "Atlanta Braves" = "ATL",
  "Philadelphia Phillies" = "PHI",
  "Miami Marlins" = "MIA",
  "Milwaukee Brewers" = "MIL",
  "Chicago Cubs" = "CHC",
  "St. Louis Cardinals" = "STL",
  "Pittsburgh Pirates" = "PIT",
  "Cincinnati Reds" = "CIN",
  "Chicago White Sox" = "CHW",
  "Minnesota Twins" = "MIN",
  "Kansas City Royals" = "KCR",
  "Detroit Tigers" = "DET",
  "Cleveland Guardians" = "CLE",
  "San Francisco Giants" = "SFG",
  "San Diego Padres" = "SDP",
  "Los Angeles Dodgers" = "LAD",
  "Arizona Diamondbacks" = "ARI",
  "Colorado Rockies" = "COL"
)

pf$home_team <- team_mapping[pf$home_team]

pf$home_team[is.na(pf$home_team)] <- "CLE"

teams <- teams %>%
  rename(season = Season)

pf <- pf %>%
  rename(team_name = home_team)

teams <- left_join(teams, pf)

#per game stats
#this wont be perfect because I can't find a way to get game by game stats,
#so instead, I'll use data for the teams against starting pitchers
#which might work better, because for QS I don't care how good they are against
#relievers

#trim fom pitcher data
team_rolling <- pitcher_stats |>
  select(1:75)

#another trim
team_rolling <- team_rolling |>
  select(1:15, 21:36, 39:41, 69:72)

#remove the @ symbol in opp team so we can group by opp
team_rolling$Opp <- gsub("@", "", team_rolling$Opp)

#final trim
team_rolling <- team_rolling |>
  select(3, 4, 6, 10:34)

#get only data from the starting pitcher of the game and arrange data
team_rolling <- team_rolling |>
  filter(GS == 1) |>
  arrange(season, Opp, Date)

#changing TBF to PA
team_rolling <- team_rolling %>%
  rename(team= Opp,
         PA = TBF 
         )



# Calculate cumulative and rolling totals and rate stats

# Create cumulative_ERA column (c_ERA) and running ERA that includes current
# and previous 5 games
team_rolling <- team_rolling %>%
  arrange(team, season, Date) %>%  # Ensure the data is ordered by team, season, and date
  mutate(
    # Convert IP to outs (whole number part * 3 + decimal part)
    outs_pitched = ((floor(IP) * 3) + ((IP - floor(IP)) * 10)),
    out_fraction = (IP - floor(IP)) / .3,
    
    # Convert back to innings
    IP_corrected = (floor(IP) + out_fraction)
  ) %>%
  group_by(team, season) %>%
  mutate(
    # Cumulative outs and earned runs per player
    cumulative_outs = cumsum(outs_pitched),
    cumulative_ER = cumsum(ER),
    
    # Cumulative innings pitched
    cumulative_IP = cumulative_outs / 3,
    
    # Calculate cumulative ERA, but handle cases where IP is zero to avoid division by zero
    c_ERA = ifelse(cumulative_IP == 0, NA, (cumulative_ER * 9) / cumulative_IP)
  ) %>%
  ungroup()

# Create rolling 10-game (previous 9 + current) ERA
team_rolling <- team_rolling %>%
  group_by(team, season) %>%
  mutate(
    rolling_ERA = sapply(seq_len(n()), function(i) {
      if (i <= 10) {
        # For rows with 10 or fewer previous games, use cumulative ERA
        c_ERA[i]
      } else {
        # Manually subset the previous 9 games including the current one
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

# Quality starts and QS ratio
team_rolling <- team_rolling %>%
  group_by(team, season) %>%
  mutate(
    # Cumulative QS and GS per player
    cumulative_QS = cumsum(QS),
    cumulative_GS = cumsum(GS),
    
    # Calculate cumulative QSR (QS ratio), but handle cases where IP is zero to avoid division by zero
    c_QSR = ifelse(cumulative_IP == 0, NA, (cumulative_QS) / cumulative_GS)
  ) %>%
  ungroup()

# Cumulative strikeouts and K/9
team_rolling <- team_rolling %>%
  group_by(team, season) %>%
  mutate(
    cumulative_SO = cumsum(SO),
    c_K9 = ifelse(cumulative_IP == 0, NA, (cumulative_SO * 9) / cumulative_IP)
  ) %>%
  ungroup()

# Create 10-game rolling K/9
team_rolling <- team_rolling %>%
  group_by(team, season) %>%
  mutate(
    rolling_K9 = sapply(seq_len(n()), function(i) {
      if (i <= 10) {
        # For rows with 10 or fewer previous games, use cumulative K/9
        c_K9[i]
      } else {
        # Manually subset the previous 10 games including the current one
        data <- cur_data()[(i-9):i, ]
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

# Cumulative BB/9
team_rolling <- team_rolling %>%
  group_by(team, season) %>%
  mutate(
    cumulative_BB = cumsum(BB),
    c_BB9 = ifelse(cumulative_IP == 0, NA, (cumulative_BB * 9) / cumulative_IP)
  ) %>%
  ungroup()

# Create 10-game rolling BB/9
team_rolling <- team_rolling %>%
  group_by(team, season) %>%
  mutate(
    rolling_BB9 = sapply(seq_len(n()), function(i) {
      if (i <= 10) {
        # For rows with 10 or fewer previous games, use cumulative BB/9
        c_BB9[i]
      } else {
        # Manually subset the previous 10 games including the current one
        data <- cur_data()[(i-9):i, ]
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
team_rolling <- team_rolling %>%
  group_by(team, season) %>%
  mutate(
    cumulative_H = cumsum(H),
    c_H9 = ifelse(cumulative_IP == 0, NA, (cumulative_H * 9) / cumulative_IP)
  ) %>%
  ungroup()

# Create 10-game rolling H/9
team_rolling <- team_rolling %>%
  group_by(team, season) %>%
  mutate(
    rolling_H9 = sapply(seq_len(n()), function(i) {
      if (i <= 10) {
        # For rows with 10 or fewer previous games, use cumulative H/9
        c_H9[i]
      } else {
        # Manually subset the previous 10 games including the current one
        data <- cur_data()[(i-9):i, ]
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
team_rolling <- team_rolling %>%
  group_by(team, season) %>%
  mutate(
    c_KBB = ifelse(cumulative_BB == 0, NA, cumulative_SO / cumulative_BB)
  ) %>%
  ungroup()

# Calculate rolling K/BB (last 10 games)
team_rolling <- team_rolling %>%
  group_by(team, season) %>%
  mutate(
    rolling_KBB = sapply(seq_len(n()), function(i) {
      if (i <= 10) {
        # For rows with 10 or fewer previous games, use cumulative K/BB
        c_KBB[i]
      } else {
        # Manually subset the previous 10 games including the current one
        data <- cur_data()[(i-9):i, ]
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
team_rolling <- team_rolling %>%
  group_by(team, season) %>%
  mutate(
    cumulative_HR = cumsum(HR),
    c_HR9 = ifelse(cumulative_IP == 0, NA, (cumulative_HR * 9) / cumulative_IP)
  ) %>%
  ungroup()

# Calculate rolling HR/9 (last 10 games)
team_rolling <- team_rolling %>%
  group_by(team, season) %>%
  mutate(
    rolling_HR9 = sapply(seq_len(n()), function(i) {
      if (i <= 10) {
        # For rows with 10 or fewer previous games, use cumulative HR/9
        c_HR9[i]
      } else {
        # Manually subset the previous 10 games including the current one
        data <- cur_data()[(i-9):i, ]
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

# Calculate cumulative R9
team_rolling <- team_rolling %>%
  group_by(team, season) %>%
  mutate(
    cumulative_R = cumsum(R),
    c_R9 = ifelse(cumulative_IP == 0, NA, (cumulative_R * 9) / cumulative_IP)
  ) %>%
  ungroup()

# Calculate rolling R9 (last 10 games)
team_rolling <- team_rolling %>%
  group_by(team, season) %>%
  mutate(
    rolling_R9 = sapply(seq_len(n()), function(i) {
      if (i <= 10) {
        # For rows with 10 or fewer previous games, use cumulative R9
        c_R9[i]
      } else {
        # Manually subset the previous 10 games including the current one
        data <- cur_data()[(i-9):i, ]
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

#join park factor to df
team_rolling <- team_rolling %>%
  rename(team_name = team)


team_rolling <- left_join(team_rolling, pf)

# Add 'T_' prefix to all column names in team_rolling
colnames(team_rolling) <- paste0("T_", colnames(team_rolling))


# Add 'T_' prefix to all column names in teams
colnames(teams) <- paste0("T_", colnames(teams))





#save DFS
saveRDS(team_rolling, "team_rolling_game_averages.rds")

saveRDS(per_season, "player_pitching_stats_by_season.rds")

saveRDS(teams, "team_batting_by_season.rds")

saveRDS(lg_avgs, "league_starting_pitching_averages.rds")

saveRDS(starters, "all_pitcher_game_by_game_averages.rds")


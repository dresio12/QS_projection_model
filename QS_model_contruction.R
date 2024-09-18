library(baseballr)
library(tidyverse)
library(zoo)
library(data.table)
library(ggplot2)
library(httr)
library(lubridate)
library(caret)
library(randomForest)
library(Metrics)
library(car)

#load in all necessary RDS files as data frames
all_data <- readRDS("mlb_master_data.rds")
p_by_game <- readRDS("all_pitcher_game_by_game_averages.rds")
lg_sp_avg_yr <- readRDS("league_starting_pitching_averages.rds")
team_bat_by_szn <- readRDS("team_batting_by_season.rds")
team_gbg_avg <- readRDS("team_rolling_game_averages.rds")
p_by_szn <- readRDS("player_pitching_stats_by_season.rds")

set.seed(123)


#bringing league averages into df
all_data <- left_join(all_data, lg_sp_avg_yr)

######

#prior to bringing in pitcher data, need to separate games into home and away rows

#create duplicate rows for all_data split by pitcher and team

# Create the 'away' team dataframe
away_data <- all_data %>%
  select(
    game_pk, season, gameDate, officialDate, dayNight, status.abstractGameState, status.codedGameState, status.detailedState, status.statusCode,
    teams.away.score, teams.away.leagueRecord.wins, teams.away.leagueRecord.losses, teams.away.leagueRecord.pct, teams.away.team.id, teams.away.team.name,
    venue.id, venue.name, rescheduleDate, rescheduleGameDate, rescheduledFrom, rescheduledFromDate, resumeDate, resumeGameDate,
    temperature, other_weather, wind, home_plate_full_name, home_plate_id, 
    starter = away_starter, id = id1,  # Renaming away_starter to starter, id1 to id
    lg_H, lg_SO, lg_HR, lg_R, lg_BB, lg_OUTSPITCHED, lg_OUTS
  ) %>%
  mutate(team = "away")

# Create the 'home' team dataframe
home_data <- all_data %>%
  select(
    game_pk, season, gameDate, officialDate, dayNight, status.abstractGameState, status.codedGameState, status.detailedState, status.statusCode,
    teams.home.score, teams.home.leagueRecord.wins, teams.home.leagueRecord.losses, teams.home.leagueRecord.pct, teams.home.team.id, teams.home.team.name,
    venue.id, venue.name, rescheduleDate, rescheduleGameDate, rescheduledFrom, rescheduledFromDate, resumeDate, resumeGameDate,
    temperature, other_weather, wind, home_plate_full_name, home_plate_id,
    starter = home_starter, id = id2,  # Renaming home_starter to starter, id2 to id
    lg_H, lg_SO, lg_HR, lg_R, lg_BB, lg_OUTSPITCHED, lg_OUTS
  ) %>%
  mutate(team = "home")

# Standardize column names for both home and away data
colnames(away_data) <- gsub("teams\\.away\\.", "", colnames(away_data))
colnames(home_data) <- gsub("teams\\.home\\.", "", colnames(home_data))

# Combine both dataframes into one and arrange by season and gameDate
all_data <- bind_rows(away_data, home_data) %>%
  arrange(season, gameDate)

saveRDS(all_data, file = "mlb_master_data.rds")

#join in starting pitching game logs
all_data <- left_join(all_data, p_by_game, 
                                      by = c('season',
                                      'starter' = 'PlayerName',
                                      'officialDate' = 'Date'))



#replace Indians with Guardians
all_data <- all_data %>%
  mutate(
   team.name = str_replace(team.name, "Cleveland Indians", "Cleveland Guardians")
  )

# Remove '@' symbol from the Opp column
all_data$Opp <- gsub("@", "", all_data$Opp)


all_data <- left_join(all_data, team_gbg_avg, by = c('officialDate' = 'T_Date',
                                                 'Opp' = 'T_team_name'
                                                 ))

all_data <- unique(all_data)

saveRDS(all_data, file = "mlb_master_data.rds")


#creating necessary factor variables
all_data$dayNight <- as.factor(all_data$dayNight)
all_data$other_weather <- as.factor(all_data$other_weather)
all_data$Opp <- as.factor(all_data$Opp)
all_data$temperature <- as.integer(all_data$temperature)

saveRDS(all_data, file = "mlb_master_data.rds")


#splitting into training and testing data
train_data <- all_data |>
  filter(season < 2023) |>
  filter(!is.na(IP),
         !is.na(c_QSR),
         !is.na(dayNight),
         !is.na(c_ERA),
         !is.infinite(c_ERA),
         !is.infinite(FIP),
         !is.na(FIP),
         !is.infinite(rolling_FIP),
         !is.na(score),
         is.na(rescheduleDate),
         !is.na(T_season)
         ) |>
  mutate(c_KBB = ifelse(is.na(c_KBB), cumulative_SO/1, c_KBB),
         rolling_KBB = ifelse(is.na(rolling_KBB), cumulative_SO/1, rolling_KBB),
         T_c_KBB = ifelse(is.na(T_c_KBB), T_cumulative_SO/1, T_c_KBB),
         T_rolling_KBB = ifelse(is.na(T_rolling_KBB), T_cumulative_SO/1, T_rolling_KBB),
         c_FB_pct = ifelse(is.na(c_FB_pct), 0, c_FB_pct),
         rolling_FB_pct = ifelse(is.na(rolling_FB_pct),  0, rolling_FB_pct),
         c_GB_pct = ifelse(is.na(c_GB_pct), 0, c_GB_pct),
         rolling_GB_pct = ifelse(is.na(rolling_GB_pct),  0, rolling_GB_pct)) 


test_data <- all_data |>
  filter(season > 2022) |>
  filter(!is.na(IP),
         !is.na(c_QSR),
         !is.na(dayNight),
         !is.na(c_ERA),
         !is.infinite(c_ERA),
         !is.infinite(FIP),
         !is.na(FIP),
         !is.infinite(rolling_FIP),
         !is.na(score),
         is.na(rescheduleDate),
         !is.na(T_season)) |>
  mutate(c_KBB = ifelse(is.na(c_KBB), cumulative_SO/1, c_KBB),
         rolling_KBB = ifelse(is.na(rolling_KBB), cumulative_SO/1, rolling_KBB),
         T_c_KBB = ifelse(is.na(T_c_KBB), T_cumulative_SO/1, T_c_KBB),
         T_rolling_KBB = ifelse(is.na(T_rolling_KBB), T_cumulative_SO/1, T_rolling_KBB),
         c_FB_pct = ifelse(is.na(c_FB_pct), 0, c_FB_pct),
         rolling_FB_pct = ifelse(is.na(rolling_FB_pct),  0, rolling_FB_pct),
         c_GB_pct = ifelse(is.na(c_GB_pct), 0, c_GB_pct),
         rolling_GB_pct = ifelse(is.na(rolling_GB_pct),  0, rolling_GB_pct))  

#removes the two rows of unknown weather
test_data <- subset(test_data, other_weather != "Unknown")

#remove unnecessary columns
test_data <- test_data |>
  select(-18:-23, -26)

train_data <- train_data |>
  select(-18:-23, -26)

#creating model for innings

modelIP <- lm(IP ~ dayNight + temperature + other_weather + home_plate_id + 
                cumulative_IP + c_ERA + rolling_ERA + c_K9 + rolling_K9 + c_QSR +
                c_BB9 + rolling_BB9 + c_H9 + rolling_H9 + c_KBB + rolling_KBB +
                c_HR9 + rolling_HR9 + c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                c_FB_pct + c_GB_pct + rolling_FB_pct + rolling_GB_pct + c_K_game +
                c_BB_game + c_ER_game + c_IP_game + c_HR_game + c_H_game + 
                rolling_K_game + rolling_BB_game + rolling_ER_game +
                rolling_H_game + rolling_HR_game + rolling_IP_game + c_p_game + 
                rolling_p_game + T_c_QSR + T_c_K9 + T_rolling_K9 + T_c_BB9 + 
                T_rolling_BB9 + T_c_H9 + T_rolling_H9 + T_c_KBB + T_rolling_KBB +
                T_c_HR9 + T_rolling_HR9 + T_c_FB_pct + T_rolling_FB_pct +
                T_c_GB_pct + T_rolling_GB_pct + T_c_AVG + T_rolling_AVG + 
                T_c_OBP + T_c_K_game + T_c_BB_game + T_c_ER_game + T_c_IP_game +
                T_c_HR_game + T_c_H_game + T_rolling_K_game + T_rolling_BB_game + 
                T_rolling_ER_game + T_rolling_H_game + T_rolling_HR_game + 
                + T_3yr + T_1yr,
              data = train_data)

predictionsIP <- predict(modelIP, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$IP, predictionsIP)
mae(test_data$IP[valid_indices], predictionsIP[valid_indices])

summary(modelIP)

plot(fitted(modelIP), residuals(modelIP), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelIP))
qqline(residuals(modelIP), col = "red")

outlier_testIP <- outlierTest(modelIP)

#examining outliers
IProws <- c(23118, 23117, 16001, 12561, 10925, 16528, 3134, 22772, 15963, 610)

IPoutliers <- train_data[IProws, ]

#extremely short outings that don't happen all that often, plus a few injuries
IP_train <- train_data[-IProws, ]

#back to model training  
modelIP <- lm(IP ~ dayNight + temperature + other_weather + home_plate_id + 
                cumulative_IP + c_ERA + rolling_ERA + c_K9 + rolling_K9 + c_QSR +
                rolling_BB9 + rolling_H9 + rolling_KBB +
                c_HR9 + rolling_HR9 + c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                rolling_FB_pct + rolling_GB_pct + c_K_game +
                c_BB_game + c_ER_game + c_IP_game + c_HR_game + 
                rolling_K_game + rolling_BB_game + rolling_ER_game +
                rolling_HR_game + rolling_IP_game + c_p_game + 
                T_c_QSR + T_c_K9 + T_rolling_K9 + T_c_BB9 + 
                T_rolling_BB9 + T_c_H9 + T_rolling_H9 + T_c_KBB +
                T_c_HR9 + T_rolling_HR9 + T_c_FB_pct + T_rolling_FB_pct +
                T_c_GB_pct + T_rolling_GB_pct + T_rolling_AVG + 
                T_c_OBP + T_c_K_game + T_c_BB_game + T_c_ER_game + T_c_IP_game +
                T_c_HR_game + T_c_H_game + T_rolling_K_game + 
                T_rolling_H_game + T_rolling_HR_game + 
                + T_3yr + T_1yr,
              data = IP_train)

predictionsIP <- predict(modelIP, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$IP, predictionsIP)
mae(test_data$IP[valid_indices], predictionsIP[valid_indices])

summary(modelIP)

plot(fitted(modelIP), residuals(modelIP), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelIP))
qqline(residuals(modelIP), col = "red")

outlier_testIP <- outlierTest(modelIP)

test_data$IPpredict <- predictionsIP

#back to model training  
modelIP <- lm(IP ~ dayNight + temperature + other_weather + 
                cumulative_IP + c_K9 + rolling_K9 + c_QSR +
                rolling_BB9 + rolling_H9 + rolling_KBB +
                rolling_HR9 + c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                rolling_FB_pct + c_K_game +
                c_ER_game + c_IP_game + c_HR_game + 
                rolling_K_game + rolling_BB_game + rolling_ER_game +
                rolling_HR_game + rolling_IP_game + c_p_game + 
                T_c_QSR + T_c_K9 + T_rolling_K9 + 
                T_c_H9 + T_rolling_H9 + T_c_KBB +
                T_c_HR9 + T_rolling_HR9 + T_c_FB_pct + T_rolling_FB_pct +
                T_c_GB_pct + T_rolling_GB_pct + 
                T_c_OBP + T_c_K_game + T_c_BB_game + T_c_ER_game + T_c_IP_game +
                T_c_HR_game + T_c_H_game + T_rolling_K_game + 
                T_rolling_H_game + T_rolling_HR_game + 
                + T_3yr + T_1yr,
              data = IP_train)

predictionsIP <- predict(modelIP, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$IP, predictionsIP)
mae(test_data$IP[valid_indices], predictionsIP[valid_indices])

summary(modelIP)

plot(fitted(modelIP), residuals(modelIP), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelIP))
qqline(residuals(modelIP), col = "red")

outlier_testIP <- outlierTest(modelIP)

test_data$IPpredict <- predictionsIP

#examining outliers
IProws <- c(9580, 9368, 8927, 1653, 2328, 19653, 15616, 9610, 8968, 16519 )

IPoutliers <- train_data[IProws, ]

#all of these are normal outings, need to adjust data because model is consistently
#predicting too low of a number
IP_train <- train_data |>
  filter(IP >= 4)


#back to model training  
modelIP <- lm(IP ~ dayNight + temperature + other_weather + 
                cumulative_IP + c_K9 + rolling_K9 + c_QSR +
                rolling_BB9 + rolling_H9 +
                rolling_HR9 + FIP + rolling_FIP +
                c_ER_game + 
                rolling_K_game + rolling_ER_game +
                rolling_IP_game + c_p_game + 
                T_c_QSR + T_rolling_K9 + 
                T_c_H9 + T_rolling_H9 + T_c_KBB +
                T_c_HR9 + T_rolling_HR9 +
                T_c_BB_game +
                T_c_HR_game + T_c_H_game + T_rolling_K_game + 
                T_rolling_H_game + T_rolling_HR_game 
                ,
              data = train_data)

predictionsIP <- predict(modelIP, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$IP, predictionsIP)
mae(test_data$IP[valid_indices], predictionsIP[valid_indices])

summary(modelIP)

plot(fitted(modelIP), residuals(modelIP), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelIP))
qqline(residuals(modelIP), col = "red")

outlier_testIP <- outlierTest(modelIP)

test_data$IPpredict <- predictionsIP

test_data <- test_data |>
  mutate(IPdiff = IPpredict - IP)

#setting different innings performance thresholds
#0 is bad performance, 1 is good
test_data$poor_performance <- ifelse(test_data$IP < 4, 0, 1)
test_data$predicted_perf <- ifelse(test_data$IPpredict < 4, 0, 1)

table(test_data$poor_performance, test_data$predicted_perf)


test_data$poor_performance <- ifelse(test_data$IP < 5.5, 0, 1)
test_data$predicted_perf <- ifelse(test_data$IPpredict < 5.5, 0, 1)

table(test_data$poor_performance, test_data$predicted_perf)


test_data$poor_performance <- ifelse(test_data$IP < 6, 0, 1)
test_data$predicted_perf <- ifelse(test_data$IPpredict < 6, 0, 1)


table(test_data$poor_performance, test_data$predicted_perf)

# Define ranges for poor performance categories
test_data$poor_performance <- ifelse(test_data$IP_corrected < 4, 0,
                                     ifelse(test_data$IP_corrected >= 4 & test_data$IP_corrected < 5.5, 1,
                                            ifelse(test_data$IP_corrected >= 5.5 & test_data$IP_corrected < 6, 2, 3)))

# Define ranges for predicted performance categories
test_data$predicted_perf <- ifelse(test_data$IPpredict < 4, 0,
                                   ifelse(test_data$IPpredict >= 4 & test_data$IPpredict < 5.5, 1,
                                          ifelse(test_data$IPpredict >= 5.5 & test_data$IPpredict < 6, 2, 3)))

confusion_matrix

confusion_matrix <- table(test_data$poor_performance, test_data$predicted_perf)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)




#Create model for HR
modelHR <- lm(HR ~ dayNight + temperature + other_weather + cumulative_IP + c_QSR +
                c_BB9 + rolling_BB9 + c_H9 + rolling_H9 + c_HR9 + rolling_HR9 +
                c_RA9 + rolling_RA9 + FIP + rolling_FIP + c_FB_pct + c_GB_pct +
                c_BB_game + c_ER_game + c_IP_game + c_HR_game + c_H_game + 
                rolling_K_game + rolling_ER_game + rolling_HR_game + 
                rolling_IP_game + c_p_game + T_rolling_K9 + T_c_BB9 + 
                T_rolling_BB9 + T_c_H9 + T_rolling_H9 + T_rolling_HR9 + 
                T_c_FB_pct + T_rolling_FB_pct + T_c_OBP + T_c_K_game +
                T_c_HR_game + T_rolling_K_game + T_rolling_BB_game + 
                T_rolling_ER_game + T_rolling_HR_game, 
            data = train_data)

predictionsHR <- predict(modelHR, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$HR, predictionsHR)
mae(test_data$HR[valid_indices], predictionsHR[valid_indices])

summary(modelHR)


#create model for ER
modelER <- lm(ER ~ temperature + 
                cumulative_IP + rolling_ERA + c_QSR +
                rolling_BB9 + 
                c_HR9 + rolling_HR9 + rolling_RA9 + FIP + rolling_FIP +
                c_FB_pct + c_GB_pct +
                c_HR_game + 
                rolling_K_game + rolling_ER_game +
                rolling_H_game + rolling_HR_game + 
                rolling_p_game + T_rolling_K9 + 
                T_rolling_BB9 +
                T_c_HR9 + T_rolling_HR9 + 
                T_c_ER_game + 
                T_rolling_ER_game + T_1yr,
              data = train_data)

predictionsER <- predict(modelER, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$ER, predictionsER)
mae(test_data$ER[valid_indices], predictionsER[valid_indices])

summary(modelER)

plot(fitted(modelER), residuals(modelER), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelER))
qqline(residuals(modelER), col = "red")

outlier_testER <- outlierTest(modelER)

#examining outliers
ERrows <- c(18742, 26533, 26535, 26532, 26534, 19335, 14764, 18696,
            11621, 22168)

ERoutliers <- train_data[ERrows, ]

#Create model for BB
modelBB <- lm(BB ~ dayNight + temperature + other_weather + home_plate_id + 
                cumulative_IP + c_ERA + rolling_ERA + c_K9 + rolling_K9 + c_QSR +
                c_BB9 + rolling_BB9 + c_H9 + rolling_H9 + c_KBB + rolling_KBB +
                c_HR9 + rolling_HR9 + c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                c_FB_pct + c_GB_pct + rolling_FB_pct + rolling_GB_pct + c_K_game +
                c_BB_game + c_ER_game + c_IP_game + c_HR_game + c_H_game + 
                rolling_K_game + rolling_BB_game + rolling_ER_game +
                rolling_H_game + rolling_HR_game + rolling_IP_game + c_p_game + 
                rolling_p_game + T_c_QSR + T_c_K9 + T_rolling_K9 + T_c_BB9 + 
                T_rolling_BB9 + T_c_H9 + T_rolling_H9 + T_c_KBB + T_rolling_KBB +
                T_c_HR9 + T_rolling_HR9 + T_c_FB_pct + T_rolling_FB_pct +
                T_c_GB_pct + T_rolling_GB_pct + T_c_AVG + T_rolling_AVG + 
                T_c_OBP + T_c_K_game + T_c_BB_game + T_c_ER_game + T_c_IP_game +
                T_c_HR_game + T_c_H_game + T_rolling_K_game + T_rolling_BB_game + 
                T_rolling_ER_game + T_rolling_H_game + T_rolling_HR_game + 
                + T_3yr + T_1yr,
            data = train_data)

predictionsBB <- predict(modelBB, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$BB, predictionsBB)
mae(test_data$BB[valid_indices], predictionsBB[valid_indices])

summary(modelBB)



#Create model for SO
modelSO <- lm(SO ~ dayNight + temperature + other_weather + home_plate_id + 
                cumulative_IP + c_ERA + rolling_ERA + c_K9 + rolling_K9 + c_QSR +
                c_BB9 + rolling_BB9 + c_H9 + rolling_H9 + c_KBB + rolling_KBB +
                c_HR9 + rolling_HR9 + c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                c_FB_pct + c_GB_pct + rolling_FB_pct + rolling_GB_pct + c_K_game +
                c_BB_game + c_ER_game + c_IP_game + c_HR_game + c_H_game + 
                rolling_K_game + rolling_BB_game + rolling_ER_game +
                rolling_H_game + rolling_HR_game + rolling_IP_game + c_p_game + 
                rolling_p_game + T_c_QSR + T_c_K9 + T_rolling_K9 + T_c_BB9 + 
                T_rolling_BB9 + T_c_H9 + T_rolling_H9 + T_c_KBB + T_rolling_KBB +
                T_c_HR9 + T_rolling_HR9 + T_c_FB_pct + T_rolling_FB_pct +
                T_c_GB_pct + T_rolling_GB_pct + T_c_AVG + T_rolling_AVG + 
                T_c_OBP + T_c_K_game + T_c_BB_game + T_c_ER_game + T_c_IP_game +
                T_c_HR_game + T_c_H_game + T_rolling_K_game + T_rolling_BB_game + 
                T_rolling_ER_game + T_rolling_H_game + T_rolling_HR_game + 
                + T_3yr + T_1yr,
              data = train_data)

predictionsSO <- predict(modelSO, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$SO, predictionsSO)
mae(test_data$SO[valid_indices], predictionsSO[valid_indices])

summary(modelSO)


#Create model for H
modelH <- lm(H ~ dayNight + temperature + other_weather + home_plate_id + 
               cumulative_IP + c_ERA + rolling_ERA + c_K9 + rolling_K9 + c_QSR +
               c_BB9 + rolling_BB9 + c_H9 + rolling_H9 + c_KBB + rolling_KBB +
               c_HR9 + rolling_HR9 + c_RA9 + rolling_RA9 + FIP + rolling_FIP +
               c_FB_pct + c_GB_pct + rolling_FB_pct + rolling_GB_pct + c_K_game +
               c_BB_game + c_ER_game + c_IP_game + c_HR_game + c_H_game + 
               rolling_K_game + rolling_BB_game + rolling_ER_game +
               rolling_H_game + rolling_HR_game + rolling_IP_game + c_p_game + 
               rolling_p_game + T_c_QSR + T_c_K9 + T_rolling_K9 + T_c_BB9 + 
               T_rolling_BB9 + T_c_H9 + T_rolling_H9 + T_c_KBB + T_rolling_KBB +
               T_c_HR9 + T_rolling_HR9 + T_c_FB_pct + T_rolling_FB_pct +
               T_c_GB_pct + T_rolling_GB_pct + T_c_AVG + T_rolling_AVG + 
               T_c_OBP + T_c_K_game + T_c_BB_game + T_c_ER_game + T_c_IP_game +
               T_c_HR_game + T_c_H_game + T_rolling_K_game + T_rolling_BB_game + 
               T_rolling_ER_game + T_rolling_H_game + T_rolling_HR_game + 
               + T_3yr + T_1yr,
              data = train_data)

predictionsH <- predict(modelH, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$H, predictionsH)
mae(test_data$H[valid_indices], predictionsH[valid_indices])

summary(modelH)




#testing out all models as Poisson

#Create model for IP
modelIP <- glm(IP ~ dayNight + temperature + other_weather + home_plate_id + 
                 cumulative_IP + c_ERA + rolling_ERA + c_K9 + rolling_K9 + c_QSR +
                 c_BB9 + rolling_BB9 + c_H9 + rolling_H9 + c_KBB + rolling_KBB +
                 c_HR9 + rolling_HR9 + c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                 c_FB_pct + c_GB_pct + rolling_FB_pct + rolling_GB_pct + c_K_game +
                 c_BB_game + c_ER_game + c_IP_game + c_HR_game + c_H_game + 
                 rolling_K_game + rolling_BB_game + rolling_ER_game +
                 rolling_H_game + rolling_HR_game + rolling_IP_game + c_p_game + 
                 rolling_p_game + T_c_QSR + T_c_K9 + T_rolling_K9 + T_c_BB9 + 
                 T_rolling_BB9 + T_c_H9 + T_rolling_H9 + T_c_KBB + T_rolling_KBB +
                 T_c_HR9 + T_rolling_HR9 + T_c_FB_pct + T_rolling_FB_pct +
                 T_c_GB_pct + T_rolling_GB_pct + T_c_AVG + T_rolling_AVG + 
                 T_c_OBP + T_c_K_game + T_c_BB_game + T_c_ER_game + T_c_IP_game +
                 T_c_HR_game + T_c_H_game + T_rolling_K_game + T_rolling_BB_game + 
                 T_rolling_ER_game + T_rolling_H_game + T_rolling_HR_game + 
                 + T_3yr + T_1yr,
             data = train_data,
             family = quasipoisson(link='log'))

summary(modelIP)

# Calculate McFadden's pseudo R-squared
# Remove NA values before calculating MAE
null_modelIP <- glm(IP ~ 1, data = train_data, 
                    family = quasipoisson(link = "log"))

logLik_full <- logLik(modelIP)
logLik_null <- logLik(null_modelIP)


pseudo_r2IP <- 1 - (logLik(modelIP) / logLik(null_modelIP))
pseudo_r2IP


#Create model for HR
modelHR <- glm(HR ~ dayNight + temperature + other_weather + home_plate_id + 
                 cumulative_IP + c_ERA + rolling_ERA + c_K9 + rolling_K9 + c_QSR +
                 c_BB9 + rolling_BB9 + c_H9 + rolling_H9 + c_KBB + rolling_KBB +
                 c_HR9 + rolling_HR9 + c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                 c_FB_pct + c_GB_pct + rolling_FB_pct + rolling_GB_pct + c_K_game +
                 c_BB_game + c_ER_game + c_IP_game + c_HR_game + c_H_game + 
                 rolling_K_game + rolling_BB_game + rolling_ER_game +
                 rolling_H_game + rolling_HR_game + rolling_IP_game + c_p_game + 
                 rolling_p_game + T_c_QSR + T_c_K9 + T_rolling_K9 + T_c_BB9 + 
                 T_rolling_BB9 + T_c_H9 + T_rolling_H9 + T_c_KBB + T_rolling_KBB +
                 T_c_HR9 + T_rolling_HR9 + T_c_FB_pct + T_rolling_FB_pct +
                 T_c_GB_pct + T_rolling_GB_pct + T_c_AVG + T_rolling_AVG + 
                 T_c_OBP + T_c_K_game + T_c_BB_game + T_c_ER_game + T_c_IP_game +
                 T_c_HR_game + T_c_H_game + T_rolling_K_game + T_rolling_BB_game + 
                 T_rolling_ER_game + T_rolling_H_game + T_rolling_HR_game + 
                 + T_3yr + T_1yr,
               data = train_data,
               family = possion(link='log'))

# Calculate McFadden's pseudo R-squared
null_modelHR <- glm(HR ~ 1, data = train_data, family = poisson(link = "log"))
pseudo_r2HR <- 1 - (logLik(modelHR) / logLik(null_modelHR))
pseudo_r2HR


#Create model for ER
modelER <- glm(ER ~ dayNight + temperature + other_weather + home_plate_id + 
                 cumulative_IP + c_ERA + rolling_ERA + c_K9 + rolling_K9 + c_QSR +
                 c_BB9 + rolling_BB9 + c_H9 + rolling_H9 + c_KBB + rolling_KBB +
                 c_HR9 + rolling_HR9 + c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                 c_FB_pct + c_GB_pct + rolling_FB_pct + rolling_GB_pct + c_K_game +
                 c_BB_game + c_ER_game + c_IP_game + c_HR_game + c_H_game + 
                 rolling_K_game + rolling_BB_game + rolling_ER_game +
                 rolling_H_game + rolling_HR_game + rolling_IP_game + c_p_game + 
                 rolling_p_game + T_c_QSR + T_c_K9 + T_rolling_K9 + T_c_BB9 + 
                 T_rolling_BB9 + T_c_H9 + T_rolling_H9 + T_c_KBB + T_rolling_KBB +
                 T_c_HR9 + T_rolling_HR9 + T_c_FB_pct + T_rolling_FB_pct +
                 T_c_GB_pct + T_rolling_GB_pct + T_c_AVG + T_rolling_AVG + 
                 T_c_OBP + T_c_K_game + T_c_BB_game + T_c_ER_game + T_c_IP_game +
                 T_c_HR_game + T_c_H_game + T_rolling_K_game + T_rolling_BB_game + 
                 T_rolling_ER_game + T_rolling_H_game + T_rolling_HR_game + 
                 + T_3yr + T_1yr,
               data = train_data,
               family = possion(link='log'))

# Calculate McFadden's pseudo R-squared
null_modelER <- glm(ER ~ 1, data = train_data, family = poisson(link = "log"))
pseudo_r2ER <- 1 - (logLik(modelER) / logLik(null_modelER))
pseudo_r2ER


#Create model for SO
modelSO <- glm(SO ~ dayNight + temperature + other_weather + home_plate_id + 
                 cumulative_IP + c_ERA + rolling_ERA + c_K9 + rolling_K9 + c_QSR +
                 c_BB9 + rolling_BB9 + c_H9 + rolling_H9 + c_KBB + rolling_KBB +
                 c_HR9 + rolling_HR9 + c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                 c_FB_pct + c_GB_pct + rolling_FB_pct + rolling_GB_pct + c_K_game +
                 c_BB_game + c_ER_game + c_IP_game + c_HR_game + c_H_game + 
                 rolling_K_game + rolling_BB_game + rolling_ER_game +
                 rolling_H_game + rolling_HR_game + rolling_IP_game + c_p_game + 
                 rolling_p_game + T_c_QSR + T_c_K9 + T_rolling_K9 + T_c_BB9 + 
                 T_rolling_BB9 + T_c_H9 + T_rolling_H9 + T_c_KBB + T_rolling_KBB +
                 T_c_HR9 + T_rolling_HR9 + T_c_FB_pct + T_rolling_FB_pct +
                 T_c_GB_pct + T_rolling_GB_pct + T_c_AVG + T_rolling_AVG + 
                 T_c_OBP + T_c_K_game + T_c_BB_game + T_c_ER_game + T_c_IP_game +
                 T_c_HR_game + T_c_H_game + T_rolling_K_game + T_rolling_BB_game + 
                 T_rolling_ER_game + T_rolling_H_game + T_rolling_HR_game + 
                 + T_3yr + T_1yr,
               data = train_data,
               family = possion(link='log'))

# Calculate McFadden's pseudo R-squared
null_modelSO <- glm(SO ~ 1, data = train_data, family = poisson(link = "log"))
pseudo_r2SO <- 1 - (logLik(modelSO) / logLik(null_modelSO))
pseudo_r2SO


#Create model for BB
modelBB <- glm(BB ~ dayNight + temperature + other_weather + home_plate_id + 
                 cumulative_IP + c_ERA + rolling_ERA + c_K9 + rolling_K9 + c_QSR +
                 c_BB9 + rolling_BB9 + c_H9 + rolling_H9 + c_KBB + rolling_KBB +
                 c_HR9 + rolling_HR9 + c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                 c_FB_pct + c_GB_pct + rolling_FB_pct + rolling_GB_pct + c_K_game +
                 c_BB_game + c_ER_game + c_IP_game + c_HR_game + c_H_game + 
                 rolling_K_game + rolling_BB_game + rolling_ER_game +
                 rolling_H_game + rolling_HR_game + rolling_IP_game + c_p_game + 
                 rolling_p_game + T_c_QSR + T_c_K9 + T_rolling_K9 + T_c_BB9 + 
                 T_rolling_BB9 + T_c_H9 + T_rolling_H9 + T_c_KBB + T_rolling_KBB +
                 T_c_HR9 + T_rolling_HR9 + T_c_FB_pct + T_rolling_FB_pct +
                 T_c_GB_pct + T_rolling_GB_pct + T_c_AVG + T_rolling_AVG + 
                 T_c_OBP + T_c_K_game + T_c_BB_game + T_c_ER_game + T_c_IP_game +
                 T_c_HR_game + T_c_H_game + T_rolling_K_game + T_rolling_BB_game + 
                 T_rolling_ER_game + T_rolling_H_game + T_rolling_HR_game + 
                 + T_3yr + T_1yr,
               data = train_data,
               family = possion(link='log'))

# Calculate McFadden's pseudo R-squared
null_modelBB <- glm(BB ~ 1, data = train_data, family = poisson(link = "log"))
pseudo_r2BB <- 1 - (logLik(modelBB) / logLik(null_modelBB))
pseudo_r2BB


#Create model for H
modelH <- glm(H ~ dayNight + temperature + other_weather + home_plate_id + 
                 cumulative_IP + c_ERA + rolling_ERA + c_K9 + rolling_K9 + c_QSR +
                 c_BB9 + rolling_BB9 + c_H9 + rolling_H9 + c_KBB + rolling_KBB +
                 c_HR9 + rolling_HR9 + c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                 c_FB_pct + c_GB_pct + rolling_FB_pct + rolling_GB_pct + c_K_game +
                 c_BB_game + c_ER_game + c_IP_game + c_HR_game + c_H_game + 
                 rolling_K_game + rolling_BB_game + rolling_ER_game +
                 rolling_H_game + rolling_HR_game + rolling_IP_game + c_p_game + 
                 rolling_p_game + T_c_QSR + T_c_K9 + T_rolling_K9 + T_c_BB9 + 
                 T_rolling_BB9 + T_c_H9 + T_rolling_H9 + T_c_KBB + T_rolling_KBB +
                 T_c_HR9 + T_rolling_HR9 + T_c_FB_pct + T_rolling_FB_pct +
                 T_c_GB_pct + T_rolling_GB_pct + T_c_AVG + T_rolling_AVG + 
                 T_c_OBP + T_c_K_game + T_c_BB_game + T_c_ER_game + T_c_IP_game +
                 T_c_HR_game + T_c_H_game + T_rolling_K_game + T_rolling_BB_game + 
                 T_rolling_ER_game + T_rolling_H_game + T_rolling_HR_game + 
                 + T_3yr + T_1yr,
               data = train_data,
               family = possion(link='log'))

# Calculate McFadden's pseudo R-squared
null_modelH <- glm(H ~ 1, data = train_data, family = poisson(link = "log"))
pseudo_r2H <- 1 - (logLik(modelH) / logLik(null_modelH))
pseudo_r2H










high_vif_vars <- c(
  "c_ERA", "rolling_ERA", 
  "c_H9", "rolling_H9", 
  "c_K_game", "c_ER_game", "c_IP_game", "c_H_game", 
  "rolling_H_game", "T_rolling_BB9", 
  "T_rolling_H9", "T_c_HR9", "T_rolling_HR9", "T_c_AVG", 
  "T_c_K_game", "T_c_HR_game",
  "T_rolling_BB_game", "T_rolling_HR_game"
)


# Extract high VIF variables
high_vif_data <- train_data[, ..high_vif_vars]

# Standardize the data
scaled_high_vif_data <- scale(high_vif_data)

# Perform PCA
pca_result <- prcomp(scaled_high_vif_data)

# Summary of PCA
summary(pca_result)

num_components <- 10
pca_data <- data.table(pca_result$x[, 1:num_components])

other_vars <- train_data[, !..high_vif_vars, with = FALSE]
new_data_table <- cbind(other_vars, pca_data)

model_with_pca <- lm(IP ~ dayNight + temperature + other_weather + 
                       home_plate_id + cumulative_IP + c_K9 + rolling_K9 + 
                       c_QSR + c_BB9 + rolling_BB9 + c_KBB + rolling_KBB +
                       c_HR9 + rolling_HR9 + c_RA9 + rolling_RA9 + FIP + 
                       rolling_FIP + c_FB_pct + c_GB_pct + rolling_FB_pct + 
                       rolling_GB_pct + c_BB_game + c_HR_game + rolling_K_game +
                       rolling_BB_game + rolling_ER_game + rolling_HR_game + 
                       rolling_IP_game + c_p_game + rolling_p_game + T_c_QSR + 
                       T_c_K9 + T_rolling_K9 + T_c_BB9 + T_c_H9 + T_rolling_KBB 
                     + T_c_FB_pct + T_rolling_FB_pct + T_c_GB_pct + 
                       T_rolling_GB_pct + T_rolling_AVG + T_c_OBP + T_c_BB_game 
                     + T_c_ER_game + T_c_IP_game + T_c_H_game + T_rolling_K_game
                     + T_rolling_ER_game + T_rolling_H_game + T_3yr + T_1yr +
                       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10
, data = new_data_table)

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
all_p_data <- readRDS("all_pitching_data.rds")
all_game_info <- readRDS("all_game_info.rds")
all_prob_starters <- readRDS("starter_list.rds")

set.seed(123)


#bringing league averages into df
all_game_info <- unique(all_game_info)

all_game_info <- left_join(all_game_info, all_prob_starters)

#change team name to abbreviation
# Create a lookup table for full team names and their abbreviations
team_abbreviations <- data.frame(
  full_name = c("Arizona Diamondbacks", "Atlanta Braves", "Baltimore Orioles", 
                "Boston Red Sox", "Chicago White Sox", "Chicago Cubs", 
                "Cincinnati Reds", "Cleveland Guardians", "Colorado Rockies",
                "Detroit Tigers", "Houston Astros", "Kansas City Royals", 
                "Los Angeles Angels", "Los Angeles Dodgers", "Miami Marlins", 
                "Milwaukee Brewers", "Minnesota Twins", "New York Yankees", 
                "New York Mets", "Oakland Athletics", "Philadelphia Phillies",
                "Pittsburgh Pirates", "San Diego Padres", "San Francisco Giants", 
                "Seattle Mariners", "St. Louis Cardinals", "Tampa Bay Rays", 
                "Texas Rangers", "Toronto Blue Jays", "Washington Nationals", 
                "Cleveland Indians"),
  abbreviation = c("ARI", "ATL", "BAL", "BOS", "CHW", 
                   "CHC", "CIN", "CLE", "COL", "DET", 
                   "HOU", "KCR", "LAA", "LAD", "MIA", 
                   "MIL", "MIN", "NYY", "NYM", "OAK", 
                   "PHI", "PIT", "SDP", "SFG", "SEA",
                   "STL", "TBR", "TEX", "TOR", "WSN", 
                   "CLE")
)

# Replace the full team names in all_game_info$team with the abbreviations
all_game_info$team <- team_abbreviations$abbreviation[match(all_game_info$team, team_abbreviations$full_name)]

all_game_info$game_date <- as.Date(all_game_info$game_date)

all_data <- left_join(all_game_info, all_p_data, by = c('game_date' = 'Date',
                                                           'fullName' = 'PlayerName',
                                                           'team' = 'Team'
                                                           ))

#filter out NA rows (lose 3500 starts, not horrible)
all_data <- all_data |>
  filter(!is.na(playerid))

all_data <- unique(all_data)

######


#creating necessary factor variables
all_data$dayNight <- as.factor(all_data$dayNight)
all_data$other_weather <- as.factor(all_data$other_weather)
all_data$temperature <- as.integer(all_data$temperature)
all_data$home_plate_full_name <- as.factor(all_data$home_plate_full_name)

saveRDS(all_data, file = "mlb_master_data.rds")

all_data <- readRDS('mlb_master_data.rds')
#splitting into training and testing data
train_data <- all_data |>
  filter(season < 2023) |>
  filter(!is.na(IP),
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
  filter(!is.na(IP)) |>
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

test_data <- test_data %>%
  mutate(
    # Convert IP to outs (whole number part * 3 + decimal part)
    new_outs_pitched = ((floor(IP) * 3) + ((IP - floor(IP)) * 10)),
    new_out_fraction = (IP - floor(IP)) / .3,
    
    # Convert back to innings
    new_IP_corrected = (floor(IP) + out_fraction))

train_data <- train_data |>
  filter(!is.na(c_ERA))

#creating model for innings
#this is the full starting list of variables
modelIP <- lm(IP ~ temperature + other_weather + 
                cumulative_IP + c_ERA + rolling_ERA + cumulative_SO + c_K9 + rolling_K9 + c_QSR +
                rolling_QSR + c_BB9 + rolling_BB9 + c_H9 + rolling_H9 + c_KBB + rolling_KBB +
                c_HR9 + rolling_HR9 + c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                c_FB_pct + c_GB_pct + rolling_FB_pct + rolling_GB_pct + c_K_game +
                c_BB_game + c_ER_game + c_IP_game + c_HR_game + c_H_game + 
                rolling_K_game + rolling_BB_game + rolling_ER_game +
                rolling_H_game + rolling_HR_game + rolling_IP_game + c_p_game + 
                rolling_p_game + T_c_RAVG + T_rolling_AVG +
                T_c_QSR + T_c_K9 + T_rolling_K9 + T_c_BB9 + 
                T_rolling_BB9 + T_c_H9 + T_rolling_H9 + T_c_KBB + T_rolling_KBB +
                T_c_HR9 + T_rolling_HR9 + T_c_FB_pct + T_c_HR9 + T_rolling_HR9 +
                T_rolling_FB_pct + T_c_FB_pct + T_rolling_FB_pct + 
                T_c_GB_pct + T_rolling_GB_pct + T_c_AVG + T_rolling_AVG + 
                T_3yr + T_1yr,
              data = train_data)

predictionsIP <- predict(modelIP, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$new_IP_corrected, predictionsIP)
mae(test_data$new_IP_corrected[valid_indices], predictionsIP[valid_indices])

summary(modelIP)

plot(fitted(modelIP), residuals(modelIP), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelIP))
qqline(residuals(modelIP), col = "red")

outlier_testIP <- outlierTest(modelIP)

train_dataIP <- train_data[-10741, ]

test_data$IPpredict <- predictionsIP


#back to model training  
modelIP <- lm(IP ~ temperature + other_weather + 
                cumulative_IP + cumulative_SO + c_K9 + c_QSR +
                rolling_QSR + c_H9 + c_KBB + rolling_KBB +
                c_HR9 + rolling_HR9 + c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                c_FB_pct + c_K_game + c_BB_game + c_IP_game + c_H_game + 
                rolling_K_game + rolling_BB_game +
                rolling_H_game + rolling_IP_game + c_p_game + 
                rolling_p_game + T_rolling_AVG + T_c_QSR + T_c_BB9 + 
                T_rolling_BB9 + T_c_H9 + T_rolling_H9 + T_c_KBB + T_rolling_KBB +
                T_c_HR9 + T_c_FB_pct + T_c_HR9 + T_rolling_FB_pct,
              data = train_dataIP)

predictionsIP <- predict(modelIP, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$new_IP_corrected, predictionsIP)
mae(test_data$new_IP_corrected[valid_indices], predictionsIP[valid_indices])

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
  mutate(IPdiff = IPpredict - new_IP_corrected)

#setting different innings performance thresholds
#0 is bad performance, 1 is good
test_data$poor_performance <- ifelse(test_data$new_IP_corrected < 4, 0, 1)
test_data$predicted_perf <- ifelse(test_data$IPpredict < 4, 0, 1)

table(test_data$poor_performance, test_data$predicted_perf)


test_data$poor_performance <- ifelse(test_data$new_IP_corrected < 5.5, 0, 1)
test_data$predicted_perf <- ifelse(test_data$IPpredict < 5.5, 0, 1)

table(test_data$poor_performance, test_data$predicted_perf)


test_data$poor_performance <- ifelse(test_data$new_IP_corrected < 6, 0, 1)
test_data$predicted_perf <- ifelse(test_data$IPpredict < 6, 0, 1)


table(test_data$poor_performance, test_data$predicted_perf)

# Define ranges for poor performance categories
test_data$poor_performance <- ifelse(test_data$new_IP_corrected < 4, 0,
                                     ifelse(test_data$new_IP_corrected >= 4 & test_data$new_IP_corrected < 5.33, 1,
                                            ifelse(test_data$new_IP_corrected >= 5.33 & test_data$new_IP_corrected < 6, 2, 3)))

# Define ranges for predicted performance categories
test_data$predicted_perf <- ifelse(test_data$IPpredict < 4, 0,
                                   ifelse(test_data$IPpredict >= 4 & test_data$IPpredict < 5.33, 1,
                                          ifelse(test_data$IPpredict >= 5.33 & test_data$IPpredict < 6, 2, 3)))

confusion_matrix <- table(test_data$poor_performance, test_data$predicted_perf)

confusion_matrix

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)




#Create model for HR
modelHR <- lm(HR ~ temperature + other_weather + c_K9 + c_QSR +
                rolling_QSR + rolling_H9 +
                c_HR9 + rolling_HR9 + FIP + rolling_FIP +
                c_FB_pct + rolling_GB_pct + c_K_game +
                + c_IP_game + c_HR_game + 
                rolling_K_game +
                rolling_H_game + rolling_HR_game + c_p_game + 
                rolling_p_game +
                T_c_QSR + T_c_K9 + T_c_H9 +
                T_c_HR9 + T_rolling_HR9 + T_c_FB_pct + T_c_HR9 + T_rolling_HR9 +
                T_rolling_FB_pct + T_rolling_FB_pct + 
                T_rolling_GB_pct + T_c_AVG,
            data = train_data)

predictionsHR <- predict(modelHR, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$HR, predictionsHR)
mae(test_data$HR[valid_indices], predictionsHR[valid_indices])

summary(modelHR)

plot(fitted(modelHR), residuals(modelHR), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelHR))
qqline(residuals(modelHR), col = "red")

outlier_testHR <- outlierTest(modelHR)

test_data$HRpredict <- predictionsHR

test_data <- test_data |>
  mutate(HRdiff = HRpredict - HR)



#create model for ER
modelER <- lm(ER ~ temperature + other_weather + c_ERA + c_K9 + c_QSR +
                rolling_QSR + c_BB9 + rolling_BB9 + c_H9 + c_KBB + rolling_KBB +
                FIP + rolling_FIP + c_ER_game + c_IP_game + 
                rolling_K_game + rolling_HR_game + rolling_IP_game + c_p_game + 
                rolling_p_game + T_c_QSR +
                T_c_HR9 + T_c_FB_pct + T_c_HR9 +
                T_rolling_FB_pct + T_c_FB_pct + T_rolling_FB_pct,
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
ERrows <- c(11138, 15062, 11783)

train_dataER <- train_data[-ERrows, ]

modelER <- lm(ER ~ temperature + other_weather + c_ERA + c_K9 + c_QSR +
                rolling_QSR + c_BB9 + rolling_BB9 + c_H9 + c_KBB + rolling_KBB +
                FIP + rolling_FIP + c_ER_game + c_IP_game + 
                rolling_K_game + rolling_HR_game + rolling_IP_game + c_p_game + 
                rolling_p_game + T_c_QSR +
                T_c_HR9 + T_c_FB_pct + T_c_HR9 +
                T_rolling_FB_pct + T_c_FB_pct + T_rolling_FB_pct,
              data = train_dataER)

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


test_data$ERpredict <- predictionsER

test_data <- test_data |>
  mutate(ERdiff = ERpredict - ER)

#Create model for BB
modelBB <- lm(BB ~ temperature + other_weather + 
                cumulative_IP + c_ERA + c_K9 + rolling_K9 + c_QSR +
                rolling_QSR + c_BB9 + rolling_BB9 + c_KBB + rolling_KBB +
                rolling_HR9 + rolling_FIP +
                c_BB_game + c_ER_game + c_IP_game + 
                rolling_K_game + rolling_BB_game + c_p_game + 
                rolling_p_game +
                T_c_QSR + T_c_BB9 + 
                T_rolling_KBB +
                T_c_HR9,
            data = train_data)

predictionsBB <- predict(modelBB, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$BB, predictionsBB)
mae(test_data$BB[valid_indices], predictionsBB[valid_indices])

summary(modelBB)

plot(fitted(modelBB), residuals(modelBB), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelBB))
qqline(residuals(modelBB), col = "red")

outlier_testBB <- outlierTest(modelBB)

BBrows <- c(10741, 15299, 1331, 11784, 5924, 5812, 10617, 5697, 147, 16981)

train_dataBB <- train_data[-BBrows, ]

modelBB <- lm(BB ~ temperature + other_weather + 
                cumulative_IP + c_ERA + c_K9 + rolling_K9 + c_QSR +
                rolling_QSR + c_BB9 + rolling_BB9 + c_KBB + rolling_KBB +
                rolling_HR9 + rolling_FIP +
                c_BB_game + c_ER_game + c_IP_game + 
                rolling_K_game + rolling_BB_game + c_p_game + 
                rolling_p_game +
                T_c_QSR + T_c_BB9 + 
                T_rolling_KBB +
                T_c_HR9,
              data = train_dataBB)

predictionsBB <- predict(modelBB, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$BB, predictionsBB)
mae(test_data$BB[valid_indices], predictionsBB[valid_indices])

summary(modelBB)

plot(fitted(modelBB), residuals(modelBB), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelBB))
qqline(residuals(modelBB), col = "red")


BBrows <- c(12602, 13732, 12173, 11895, 11619, 113, 11739, 1443, 11475, 6040)

train_dataBB <- train_dataBB[-BBrows, ]

modelBB <- lm(BB ~ temperature + other_weather + 
                c_ERA + c_K9 + rolling_K9 + c_QSR +
                rolling_QSR + c_BB9 + rolling_BB9 + c_KBB + rolling_KBB +
                rolling_HR9 + rolling_FIP +
                c_BB_game + c_IP_game + 
                rolling_BB_game + c_p_game + 
                rolling_p_game +
                T_c_QSR + T_c_BB9 + 
                T_rolling_KBB +
                T_c_HR9,
              data = train_dataBB)

predictionsBB <- predict(modelBB, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$BB, predictionsBB)
mae(test_data$BB[valid_indices], predictionsBB[valid_indices])

summary(modelBB)

plot(fitted(modelBB), residuals(modelBB), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelBB))
qqline(residuals(modelBB), col = "red")


test_data$BBpredict <- predictionsBB

test_data <- test_data |>
  mutate(BBdiff = BBpredict - BB)

outlier_testBB <- outlierTest(modelBB)

BBrows <- c(1205, 1450, 5935, 6038, 14226, 8595, 14455, 12266, 546)

train_dataBB <- train_dataBB[-BBrows, ]

modelBB <- lm(BB ~ temperature + other_weather + 
                c_ERA + c_K9 + rolling_K9 + c_QSR +
                rolling_QSR + c_BB9 + rolling_BB9 + c_KBB + rolling_KBB +
                rolling_HR9 + rolling_FIP +
                c_IP_game + 
                rolling_BB_game + c_p_game + 
                T_c_QSR + T_c_BB9 + 
                T_rolling_KBB +
                T_c_HR9,
              data = train_dataBB)

predictionsBB <- predict(modelBB, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$BB, predictionsBB)
mae(test_data$BB[valid_indices], predictionsBB[valid_indices])

summary(modelBB)

plot(fitted(modelBB), residuals(modelBB), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelBB))
qqline(residuals(modelBB), col = "red")

test_data$BBpredict <- predictionsBB

test_data <- test_data |>
  mutate(BBdiff = BBpredict - BB)

outlier_testBB <- outlierTest(modelBB)

BBrows <- c(10734, 15290, 1329, 11776, 5919, 5808, 10611, 5694, 113, 146)

train_dataBB <- train_dataBB[-BBrows, ]

modelBB <- lm(BB ~ temperature + other_weather + 
                c_ERA + c_K9 + rolling_K9 + c_QSR +
                rolling_QSR + c_BB9 + rolling_BB9 + c_KBB + rolling_KBB +
                rolling_HR9 + rolling_FIP +
                c_IP_game + 
                rolling_BB_game + c_p_game + 
                T_c_QSR + T_c_BB9 + 
                T_rolling_KBB +
                T_c_HR9,
              data = train_dataBB)

predictionsBB <- predict(modelBB, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$BB, predictionsBB)
mae(test_data$BB[valid_indices], predictionsBB[valid_indices])

summary(modelBB)

plot(fitted(modelBB), residuals(modelBB), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelBB))
qqline(residuals(modelBB), col = "red")

test_data$BBpredict <- predictionsBB

test_data <- test_data |>
  mutate(BBdiff = BBpredict - BB)


#
#
#Create model for SO
modelSO <- lm(SO ~ temperature + other_weather + 
                rolling_ERA + cumulative_SO + c_K9 + rolling_K9 + c_QSR +
                rolling_QSR + c_BB9 + rolling_BB9 + c_KBB + rolling_KBB +
                c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                c_K_game +
                c_BB_game + c_IP_game + c_H_game + 
                rolling_K_game + rolling_BB_game + rolling_ER_game +
                rolling_H_game + rolling_HR_game + rolling_IP_game + c_p_game + 
                rolling_p_game + T_c_RAVG +
                T_c_QSR + T_c_K9 + T_c_BB9 + 
                + T_c_KBB +
                + T_c_FB_pct +
                T_rolling_FB_pct + T_rolling_FB_pct + 
                T_c_GB_pct + T_c_AVG,
              data = train_data)

predictionsSO <- predict(modelSO, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$SO, predictionsSO)
mae(test_data$SO[valid_indices], predictionsSO[valid_indices])

summary(modelSO)

plot(fitted(modelSO), residuals(modelSO), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelSO))
qqline(residuals(modelSO), col = "red")

outlier_testSO <- outlierTest(modelSO)

SOrows <- c(10741, 5294, 11784, 15299)

train_dataSO <- train_data[-SOrows, ]

modelSO <- lm(SO ~ temperature + other_weather + 
                rolling_ERA + cumulative_SO + c_K9 + rolling_K9 + c_QSR +
                rolling_QSR + c_BB9 + rolling_BB9 + c_KBB + rolling_KBB +
                c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                c_K_game +
                c_BB_game + c_IP_game + c_H_game + 
                rolling_K_game + rolling_BB_game + rolling_ER_game +
                rolling_H_game + rolling_HR_game + rolling_IP_game + c_p_game + 
                rolling_p_game + T_c_RAVG +
                T_c_QSR + T_c_K9 + T_c_BB9 + 
                + T_c_KBB +
                + T_c_FB_pct +
                T_rolling_FB_pct + T_rolling_FB_pct + 
                T_c_GB_pct + T_c_AVG,
              data = train_dataSO)

predictionsSO <- predict(modelSO, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$SO, predictionsSO)
mae(test_data$SO[valid_indices], predictionsSO[valid_indices])

summary(modelSO)

plot(fitted(modelSO), residuals(modelSO), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelSO))
qqline(residuals(modelSO), col = "red")

test_data$SOpredict <- predictionsSO

test_data <- test_data |>
  mutate(SOdiff = SOpredict - SO)


outlier_testSO <- outlierTest(modelSO)

SOrows <- c(5923, 1331, 5696, 5811)

train_dataSO <- train_dataSO[-SOrows, ]

modelSO <- lm(SO ~ temperature + other_weather + 
                rolling_ERA + cumulative_SO + c_K9 + rolling_K9 + c_QSR +
                rolling_QSR + c_BB9 + rolling_BB9 + c_KBB + rolling_KBB +
                c_RA9 + rolling_RA9 + FIP + rolling_FIP +
                c_K_game +
                c_BB_game + c_IP_game + c_H_game + 
                rolling_K_game + rolling_BB_game + rolling_ER_game +
                rolling_H_game + rolling_HR_game + rolling_IP_game + c_p_game + 
                rolling_p_game + T_c_RAVG +
                T_c_QSR + T_c_K9 + T_c_BB9 + 
                + T_c_KBB +
                T_c_AVG,
              data = train_dataSO)

predictionsSO <- predict(modelSO, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$SO, predictionsSO)
mae(test_data$SO[valid_indices], predictionsSO[valid_indices])

summary(modelSO)

plot(fitted(modelSO), residuals(modelSO), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelSO))
qqline(residuals(modelSO), col = "red")

test_data$SOpredict <- predictionsSO

test_data <- test_data |>
  mutate(SOdiff = SOpredict - SO)

#62% of predictions are within 2 Ks either way, which I think is good?
ex <- test_data |>
  filter(SOdiff > -2 & SOdiff < 2)


#Create model for H
modelH <- lm(H ~ temperature + other_weather + 
               c_ERA + cumulative_SO + c_K9 + c_QSR +
               rolling_QSR + c_H9 + rolling_H9 + 
               c_HR9 + c_RA9 + rolling_RA9 + FIP +
               c_K_game +
               c_BB_game + c_ER_game + c_HR_game + c_H_game + 
               rolling_K_game +
               rolling_H_game + rolling_IP_game + c_p_game + 
               rolling_p_game + T_c_RAVG + T_rolling_AVG +
               T_c_QSR + T_c_K9 + T_rolling_K9 + T_c_BB9 + 
               T_rolling_BB9 + T_c_H9 + T_c_KBB +
               T_c_HR9 + T_c_FB_pct + T_c_HR9 +
               T_rolling_FB_pct + T_c_FB_pct + T_rolling_FB_pct + 
               T_c_GB_pct + T_rolling_GB_pct + T_c_AVG,
              data = train_data)

predictionsH <- predict(modelH, newdata = test_data)

# Remove NA values before calculating MAE
valid_indices <- complete.cases(test_data$H, predictionsH)
mae(test_data$H[valid_indices], predictionsH[valid_indices])

summary(modelH)

plot(fitted(modelH), residuals(modelH), 
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")
qqnorm(residuals(modelH))
qqline(residuals(modelH), col = "red")

test_data$Hpredict <- predictionsH

test_data <- test_data |>
  mutate(Hdiff = Hpredict - H)

outlier_testH <- outlierTest(modelH)

#61.6% of predictions are within 2 Ks either way, which I think is good?
ex2 <- test_data |>
  filter(Hdiff > -2 & Hdiff < 2)

#generating Quality Start prediction probability and Yes/No recommendation
#Softened condition for a QS to reduce perfect separation
test_data$predicted_QS <- ifelse(test_data$IPpredict >= 5.3 & test_data$ERpredict <= 3.3, 1, 0)

train_data$predicted_QS <- ifelse(train_data$IPpredict >= 6 & train_data$ERpredict <= 3, 1, 0)

colnames(train_data)[colnames(train_data) == "IP"] <- "IPpredict"
colnames(train_data)[colnames(train_data) == "H"] <- "Hpredict"
colnames(train_data)[colnames(train_data) == "SO"] <- "SOpredict"
colnames(train_data)[colnames(train_data) == "HR"] <- "HRpredict"
colnames(train_data)[colnames(train_data) == "BB"] <- "BBpredict"
colnames(train_data)[colnames(train_data) == "ER"] <- "ERpredict"



# Train the model using actual values from train_data
model_QS <- glm(predicted_QS ~ IPpredict + Hpredict + SOpredict + HRpredict +
                  BBpredict + ERpredict, 
                data = train_data, family = "binomial",
                weights = ifelse(train_data$predicted_QS == 1, 2, 1))

# Predict QS probability using predicted values in test_data
test_data$QS_prob <- predict(model_QS, 
                             newdata = test_data[, c("IPpredict", "Hpredict", "SOpredict", "HRpredict", "BBpredict", "ERpredict")], 
                             type = "response")


threshold <- 0.05
test_data$QS_recommendation <- ifelse(test_data$QS_prob >= threshold, 1, 0)

test_data <- test_data |>
  mutate(QSdiff = QS_recommendation - QS)

#accuracy test
qs_diff_counts <- table(test_data$QSdiff)

qs_diff_counts

table(test_data$QS_recommendation)

#there are 1199 true positives, 903 false positives, 4445 true negatives, 
#and 1778 false negatives. a good start for now
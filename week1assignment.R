library(tidyverse)
library(faraway)
library(simex)
library(magrittr)
library(moments)
library(MASS)
library(GGally)

money.ball <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621week1\\moneyball-training-data.csv')

#Missing

money.ball %>%
  map_dbl(~sum(is.na(.))/nrow(money.ball))

print(nrow(money.ball))

#Drop Team_Baserun_cs (missing 34%), Team_Batting_HBP (missing 91%)

money.ball %<>%
  select(-TEAM_BASERUN_CS, -TEAM_BATTING_HBP)

#Remove 19 teams missing too much info, namely (TEAM_BATTING_SO, TEAM_RUNNING_SB, TEAM_PITCHING_SO)

money.ball %<>%
  filter(TEAM_BATTING_SO != 0)

#Remove 0 win team, obvious mistake along with 3 others with values so far skewed they must be mistakes or scaling errors

money.ball %<>%
  filter(!INDEX %in% c(1347, 2380, 1494, 1769))


#Fill missing values with median

money.ball %<>%
  map_df(~ifelse(is.na(.), median(., na.rm=TRUE), .))

pairs(money.ball[, -1])
cor(money.ball[, -1])
ggpairs(money.ball, lower=list(continuous='smooth'))

#Remove index for running lm

money.ball %<>%
  select(-INDEX)

#First model -- USE EVERYTHING
l.all <- lm(TARGET_WINS ~ ., money.ball)
summary(l.all)

plot(l.all)

#Assumption #1: Residuals of the model are nearly normal.
#Closish -- there are a few upper and lower deviations but not immensly so.

#Assumption #2: Variability of ht eresiduals is nearly constant
#This is a fair assumption

#Assumption #3: Residuals are independent. 
#Should be done because data was collected over time but the data set does not contain this information

#Assumption #4: each variable is linearly related to the outcome
ggplot() +
  geom_point(aes(money.ball$TEAM_FIELDING_DP, l.all$residuals)) +
  stat_smooth(aes(money.ball$TEAM_FIELDING_DP, l.all$residuals), method='lm')

#Second Model -- BACKWARDS SELECTION

l.back <- lm(TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB +
              TEAM_FIELDING_E + TEAM_FIELDING_DP, money.ball)
summary(l.back)

#Third Model -- Predictor Condensing + Backwards selection
#Combine all positive batting statistics
money.ball.2 <- money.ball %>%
  mutate(TEAM_BATTING_1B = TEAM_BATTING_H - TEAM_BATTING_2B - TEAM_BATTING_3B - TEAM_BATTING_3B - TEAM_BATTING_HR,
         TEAM_BATTING_P1 = TEAM_BATTING_1B + TEAM_BATTING_BB + TEAM_BASERUN_SB,
         TEAM_BATTING_POS = TEAM_BATTING_P1 + TEAM_BATTING_2B * 2 + TEAM_BATTING_3B * 3 + TEAM_BATTING_HR * 4) %>%
  dplyr::select(-TEAM_BATTING_H, -TEAM_BATTING_1B, -TEAM_BATTING_BB, -TEAM_BATTING_P1, -TEAM_BATTING_2B, -TEAM_BATTING_3B, -TEAM_BATTING_HR, -TEAM_BASERUN_SB) %>%
  dplyr::select(-TEAM_FIELDING_DP) %>%
  filter(row_number() != 1)

l.combine <- lm(TARGET_WINS ~ TEAM_BATTING_SO + TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E +
                  TEAM_BATTING_POS, money.ball.2)
summary(l.combine)

#Remove TEAM_PITCHING_H
plot(l.combine)








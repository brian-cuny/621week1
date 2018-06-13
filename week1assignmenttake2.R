#library(MASS)
library(tidyverse)
#library(faraway)
#library(simex)
library(magrittr)
#library(moments)
library(GGally)
library(knitr)
library(kableExtra)
library(caret)
money.ball <- read_csv('C:\\Users\\Brian\\Desktop\\GradClasses\\Summer18\\621\\621week1\\moneyball-training-data.csv')

money.ball %<>%
  select(-TEAM_BASERUN_CS, -TEAM_BATTING_HBP)

money.ball %<>%
  filter(TEAM_BATTING_SO != 0)

money.ball %<>%
  filter(!INDEX %in% c(1347, 2380, 1494, 1769))

dummy.vars <- dummyVars(~ ., data = money.ball[, c(-1, -2)])
train.dummy <- predict(dummy.vars, money.ball[, c(-1, -2)])


pre.process <- preProcess(train.dummy, method='bagImpute')
imputed.data <- predict(pre.process, train.dummy)

money.ball %<>%
  mutate(TEAM_BATTING_SO = imputed.data[, 6],
         TEAM_BASERUN_SB = imputed.data[, 7],
         TEAM_PITCHING_SO = imputed.data[, 11],
         TEAM_FIELDING_DP = imputed.data[, 13]
  )

money.ball %<>%
  select(-INDEX)


# regression 1 -- everything ----------------------------------------------

l.all <- lm(TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR +
              TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + log(TEAM_PITCHING_H) +
              TEAM_PITCHING_HR + log(TEAM_PITCHING_BB) + log(TEAM_PITCHING_SO) + TEAM_FIELDING_E + 
              TEAM_FIELDING_DP, money.ball)
summary(l.all) #0.3232

pairs(TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR +
      TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + log(TEAM_PITCHING_H) +
      TEAM_PITCHING_HR + log(TEAM_PITCHING_BB) + log(TEAM_PITCHING_SO) + TEAM_FIELDING_E + 
      TEAM_FIELDING_DP, money.ball) #Linear Relationship 

plot(l.all) #Not quite normal, but pretty close. Has long tails

car::residualPlots(l.all) #Homoscedacity is pretty good

faraway::vif(model.matrix(l.all)[, -1]) #Colinearity is a problem

termplot(l.all, partial.resid=TRUE, smooth=panel.smooth, term=1:13)


# backwards selection -----------------------------------------------------

l.back <- lm(TARGET_WINS ~ TEAM_BATTING_H + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR +
              TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + log(TEAM_PITCHING_H) +
              log(TEAM_PITCHING_BB) + log(TEAM_PITCHING_SO) + TEAM_FIELDING_E + 
              TEAM_FIELDING_DP, money.ball)
summary(l.back) #0.3231

#Repeated all still valid

anova(l.all, l.back)

# highly modified ---------------------------------------------------------

money.ball.2 <- money.ball %>%
  mutate(TEAM_BATTING_POS = TEAM_BATTING_H + TEAM_BATTING_BB + TEAM_BASERUN_SB + TEAM_BATTING_2B * 1 + 
           TEAM_BATTING_3B * 2 + TEAM_BATTING_HR * 3) %>%
  dplyr::select(-TEAM_BATTING_H, -TEAM_BATTING_BB, -TEAM_BATTING_2B, -TEAM_BATTING_3B, -TEAM_BATTING_HR, -TEAM_BASERUN_SB)

l.combine <- lm(TARGET_WINS ~ TEAM_BATTING_SO + log(TEAM_PITCHING_BB) + log(TEAM_PITCHING_SO) + 
                  TEAM_FIELDING_E + TEAM_BATTING_POS + TEAM_FIELDING_DP, money.ball.2)

summary(l.combine) #.3047

pairs(TARGET_WINS ~ TEAM_BATTING_SO + log(TEAM_PITCHING_H) + log(TEAM_PITCHING_BB) + 
        log(TEAM_PITCHING_SO) + TEAM_FIELDING_E + TEAM_BATTING_POS + TEAM_FIELDING_DP, money.ball.2) #Linear Relationship

plot(l.combine) #Multivariate Normality

car::residualPlots(l.combine) #Homoscedasticity

faraway::vif(model.matrix(l.combine)[, -1]) #Strong collinearity is a problem in 2 predictors.

car::avPlots(l.combine)

termplot(l.combine, partial.resid=TRUE, smooth=panel.smooth, term=1:6)

car::mmps(l.combine)

MASS::boxcox(l.combine, plotit=T, lambda=seq(0, 2, by=0.1))


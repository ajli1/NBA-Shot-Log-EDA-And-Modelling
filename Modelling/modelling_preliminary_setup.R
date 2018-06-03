library(tidyverse)
library(class)
library(leaps)
library(modelr)
library(glmnet)
library(stringr)
library(caret)
library(e1071)
library(tree)
library(randomForest)
library(gbm)

#create clean dataset
#remove unnecessary columns, factorize certain columns
shot_dat <- read_csv("data/unprocessed/shot_logs.csv") %>%
  select(-CLOSEST_DEFENDER_PLAYER_ID, -player_id, -GAME_ID) %>%
  mutate(LOCATION = factor(LOCATION, levels = c("A", "H")),
         W = factor(W, levels = c("W", "L")),
         SHOT_RESULT = factor(SHOT_RESULT, levels = c("made", "missed")),
         PTS_TYPE = factor(PTS_TYPE, levels = c(2, 3)),
         PERIOD = factor(PERIOD, levels = c(1, 2, 3, 4, 5, 6, 7)))

#create opposing team variable
shot_dat <- shot_dat %>%
  mutate(opposing_team = factor(str_sub(MATCHUP, start = -3)))

#check for 30 teams
shot_dat %>% distinct(opposing_team)

#replace na's with 0
shot_dat <- shot_dat %>% replace_na(replace = list(SHOT_CLOCK = 0))

#change game clock to just seconds
shot_dat <- shot_dat %>%
  mutate(min = as.numeric(str_sub(GAME_CLOCK, start = 1, end = 2)),
         sec = as.numeric(str_sub(GAME_CLOCK, start = 4, end = 5)),
         GAME_CLOCK_sec = 60*min + sec)

#remove unnecessary columns
shot_dat <- shot_dat %>%
  select(-SHOT_RESULT, -MATCHUP, -W, -FINAL_MARGIN, -GAME_CLOCK, -min, -sec, -PTS)

#save as csv to processed data folder
#write_csv(x = shot_dat, path = "data/processed/shot_dat_processed.csv", col_names = T)

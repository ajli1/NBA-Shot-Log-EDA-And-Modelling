library(tidyverse)
library(caret)
library(MASS)
library(class)
library(leaps)
library(modelr)
library(glmnet)

#read in processed data and factorize variables
shot_dat <- read_csv("data/processed/shot_dat_processed.csv") %>%
  mutate(LOCATION = factor(LOCATION, levels = c("A", "H")),
         PERIOD = factor(PERIOD, levels = c(1, 2, 3, 4, 5, 6, 7)),
         PTS_TYPE = factor(PTS_TYPE, levels = c(2, 3)),
         CLOSEST_DEFENDER = factor(CLOSEST_DEFENDER),
         FGM = factor(FGM, levels = c(0, 1)),
         player_name = factor(player_name),
         opposing_team = factor(opposing_team)) %>% dplyr::select(-CLOSEST_DEFENDER, -player_name)

#set seed
set.seed(1)

#leave out 20% of data
shot_dat_train <- shot_dat %>% sample_frac(.8)
shot_dat_test <- shot_dat %>% setdiff(shot_dat_train)

#logistic model

#create log model
log_mod <- glm(FGM ~ ., family = binomial, data = shot_dat_train)

#create predictions
log_mod_pred <- as_tibble(x = predict(log_mod, newdata = shot_dat_test, type = "response"))
log_mod_pred <- log_mod_pred %>%
  mutate(pred = if_else(value > .5, 1, 0))

#confusion matrix
confusionMatrix(data = log_mod_pred$pred,
                reference = shot_dat_test$FGM)
#accuracy = .6090

#lda

#create lda model
lda_mod <- lda(FGM ~ ., data = shot_dat_train)

#create predictions
lda_mod_pred <- predict(lda_mod, shot_dat_test, type = "response")

#extract probabilities
lda_mod_prob <- as_tibble(lda_mod_pred$posterior[,2]) %>%
  mutate(pred = if_else(value > .5, 1, 0),
         pred = factor(pred, levels = c(0, 1)))

#confusion matrix
confusionMatrix(data = lda_mod_prob$pred,
                reference = shot_dat_test$FGM)
#accuracy = .6087

#qda

#create qda model
qda_mod <- qda(FGM ~ ., data = shot_dat_train, type = "response")

#create predictions
qda_mod_pred <- predict(qda_mod, shot_dat_test, type = "response")

#extract probabilities
qda_mod_prob <- as_tibble(qda_mod_pred$posterior[,2]) %>%
  mutate(pred = if_else(value > .5, 1, 0),
         pred = factor(pred, levels = c(0, 1)))

#confusion matrix
confusionMatrix(data = qda_mod_prob$pred,
                reference = shot_dat_test$FGM)
#accuracy = .5585
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

#read in processed data and factorize variables
shot_dat <- read_csv("data/processed/shot_dat_processed.csv") %>%
  mutate(LOCATION = factor(LOCATION, levels = c("A", "H")),
         PERIOD = factor(PERIOD, levels = c(1, 2, 3, 4, 5, 6, 7)),
         PTS_TYPE = factor(PTS_TYPE, levels = c(2, 3)),
         CLOSEST_DEFENDER = factor(CLOSEST_DEFENDER),
         FGM = factor(FGM, levels = c(0, 1)),
         player_name = factor(player_name),
         opposing_team = factor(opposing_team)) %>% select(-CLOSEST_DEFENDER, -player_name)

#set seed
set.seed(1)

#leave out 20% of data
shot_dat_train <- shot_dat %>% sample_frac(.8)
shot_dat_test <- shot_dat %>% setdiff(shot_dat_train)

#bagging

#bag mod: mtry = 11 (bagging)
bag_mod <- randomForest(FGM ~ ., data = shot_dat_train, mtry = 11, importance = T)

# saveRDS(bag_mod, "results/bag_mod.rds")

#create predictions
yhat_bag <- predict(bag_mod, newdata = shot_dat_test, type = "class")
confusionMatrix(data = yhat_bag, 
                reference = shot_dat_test$FGM)
#acc = .6031

#randomforest
#mtry = default value, sqrt(p)
rf_mod <- randomForest(FGM ~ ., data = shot_dat_train, importance = T)

# saveRDS(rf_mod, "results/rf_mod.rds")

#create predictions
yhat_rf <- predict(rf_mod, newdat = shot_dat_test, type = "class")
confusionMatrix(data = yhat_rf,
                reference = shot_dat_test$FGM)
#acc = .6024

#boosting
#un-factorize FGM variable
train_charac <- shot_dat_train %>% mutate(FGM = as.character(FGM))
test_charac <- shot_dat_test %>% mutate(FGM = as.character(FGM))

#boosting model
boost_mod <- gbm(FGM ~ ., data = train_charac, distribution = "bernoulli",
                        n.trees = 5000, interaction.depth = 4)
# saveRDS(boost_mod, "results/boost_mod.rds")

#add boosting predictions, set equal to 1 if over .5, 0 otherwise
yhat_boost <- as_tibble(x = predict(boost_mod, newdata = test_charac, n.trees = 5000, type = "response"))
yhat_boost <- yhat_boost %>%
  mutate(pred = if_else(value > .5, 1, 0),
         pred = factor(pred, levels = c(0, 1)))

#confusion matrix
confusionMatrix(data = yhat_boost$pred,
                reference = shot_dat_test$FGM)
#accuracy = .6195
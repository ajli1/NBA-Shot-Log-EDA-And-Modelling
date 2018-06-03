library(tree)
library(tidyverse)
library(caret)
library(e1071)
library(randomForest)
library(gbm)

#read in processed data and factorize variables
shot_dat <- read_csv("data/shot_dat_processed.csv") %>%
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

#SVM Linear

#set seed
set.seed(1)

#fit support vector classifier
tune_out <- tune(svm, FGM ~ ., data = shot_dat_train, kernel = "linear",
                 ranges = list(cost = c(.01, .1, 1, 5, 10)))


#summary statistics: use cost = 
summary(tune_out)

#generate predictions on test
pred_test <- predict(best_mod_linear, shot_dat_test)

#test error: 
confusionMatrix(data = pred_test, reference = shot_dat_test$FGM)

#SVM Radial

#set seed
set.seed(1)

#tune rad
tune_rad <- tune(svm, FGM ~ ., data = shot_dat_train, kernel = "radial",
                 ranges = list(cost = c(.01, .1, 1, 5, 10, 15),
                               gamma = c(.01, .1, 1, 2, 3)))

#best parameters: cost = , gamma = 
summary(tune_rad)

#best model
best_mod_rad <- tune_rad$best.model

#summary statistics
summary(best_mod_rad)

#generate predictions on test
pred_test_rad <- predict(best_mod_rad, newdata = shot_dat_test, type = "class")

#test error: .
confusionMatrix(data = pred_test_rad, reference = shot_dat_test$FGM)

#SVM Polynomial

#set seed
set.seed(1)

#tune poly
tune_poly <- tune(svm, FGM ~ ., data = shot_dat_train, kernel = "polynomial",
                  ranges = list(cost = c(.01, .1, 1, 5, 10, 15),
                                degree = c(.01, .1, 1, 2, 5, 10)))

#best parameters: cost = , degree = 
summary(tune_poly)

#best model
best_mod_poly <- tune_poly$best.model

#summary statistics
summary(best_mod_poly)

#generate predictions on test
pred_test_poly <- predict(best_mod_poly, newdata = shot_dat_test, type = "class")

#test error: .
confusionMatrix(data = pred_test_poly, reference = shot_dat_test$FGM)
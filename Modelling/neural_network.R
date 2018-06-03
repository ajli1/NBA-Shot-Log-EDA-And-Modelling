#load packages
library(tensorflow)
use_python("/usr/bin/python")
library(keras)
library(tidyverse)

#read in data
shot_dat_train <- read_csv("data/shot_dat_train.csv") %>%
  mutate(LOCATION = factor(LOCATION, levels = c("A", "H")),
         PERIOD = factor(PERIOD, levels = c(1, 2, 3, 4, 5, 6, 7)),
         PTS_TYPE = factor(PTS_TYPE, levels = c(2, 3)),
         opposing_team = factor(opposing_team))

shot_dat_test <- read_csv("data/shot_dat_test.csv") %>%
  mutate(LOCATION = factor(LOCATION, levels = c("A", "H")),
         PERIOD = factor(PERIOD, levels = c(1, 2, 3, 4, 5, 6, 7)),
         PTS_TYPE = factor(PTS_TYPE, levels = c(2, 3)),
         opposing_team = factor(opposing_team))

#need to implement one-hot encoding, exclude intercept
train_dat_ohe <- model.matrix(FGM ~ ., data = shot_dat_train)[,-1]
test_dat_ohe <- model.matrix(FGM ~ ., data = shot_dat_test)[,-1]

#train targets
train_targets <- shot_dat_train %>% pull(FGM)
test_targets <- shot_dat_test %>% pull(FGM)

#standardizing data
means_train_dat <- apply(train_dat_ohe, 2, mean)
std_train_dat <- apply(train_dat_ohe, 2, sd)
train_data <- scale(train_dat_ohe, center = means_train_dat, scale = std_train_dat)
test_data <- scale(test_dat_ohe, center = means_train_dat, scale = std_train_dat)

#model creation
model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = dim(train_data)[2]) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

#compile model with rmsprop optimizer and binary_crossentropy loss function
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

#comfigure optimizer
model %>% compile(
  optimizer = optimizer_rmsprop(lr = .001),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

#using custom losses and metrics
model %>% compile(
  optimizer = optimizer_rmsprop(lr = .001),
  loss = loss_binary_crossentropy,
  metrics = metric_binary_accuracy
)

#set aside validation set
val_indices <- 1:50000
x_val <- train_data[val_indices,]
partial_x_train <- train_data[-val_indices,]

y_val <- train_targets[val_indices]
partial_y_train <- train_targets[-val_indices]

#train model for 200 epochs
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 100,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)

plot(history)

saveRDS(plot(history), "results/validation1.rds")
#very little gain after 25 epochs

#retrain new network from scratch for 25 epochs
model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = dim(train_data)[2]) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

model %>% fit(train_data, train_targets, epochs = 25, batch_size = 512)
results <- model %>% evaluate(test_data, test_targets)
results
#acc = 0.6135707

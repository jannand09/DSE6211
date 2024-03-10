

library(dplyr)
library(lubridate)
library(caret)

project_data <- read.csv("project_data/project_data.csv")

rownames(project_data) <- project_data$Booking_ID
project_data <- project_data[, -1]

project_data %>%
  group_by(booking_status) %>%
  summarise(count = n())

11878 / (24360+11878)

training_ind <- createDataPartition(project_data$booking_status,
                                    p = 0.75,
                                    list = F,
                                    times = 1)

training_set <- project_data[training_ind, ]
test_set <- project_data[-training_ind, ]


training_set$booking_status <- ifelse(training_set$booking_status=="canceled",1,0)
test_set$booking_status <- ifelse(test_set$booking_status=="canceled",1,0)


###### Training Set Categorical Features

top_8_previous_not_cancelled <- training_set %>%
  group_by(no_of_previous_bookings_not_canceled) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  select(no_of_previous_bookings_not_canceled) %>%
  slice(1:8)

top_2_number_of_children <- training_set %>%
  group_by(no_of_children) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:2)

top_3_previous_cancellations <- training_set %>%
  group_by(no_of_previous_cancellations) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:3)

top_8_week_nights <- training_set %>%
  group_by(no_of_week_nights) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:8)

top_6_weekend_nights <- training_set %>%
  group_by(no_of_weekend_nights) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:6)

top_4_special_requests <- training_set %>%
  group_by(no_of_special_requests) %>%
  summarise(count =  n()) %>%
  arrange(desc(count)) %>%
  slice(1:4)


training_set$no_of_previous_bookings_not_canceled <- ifelse(
  training_set$no_of_previous_bookings_not_canceled %in% top_8_previous_not_cancelled$no_of_previous_bookings_not_canceled,
  training_set$no_of_previous_bookings_not_canceled, "8+"
)

training_set$no_of_children <- ifelse(
  training_set$no_of_children %in% top_2_number_of_children$no_of_children,
  training_set$no_of_children, "3+"
)

training_set$no_of_previous_cancellations <- ifelse(
  training_set$no_of_previous_cancellations %in% top_3_previous_cancellations$no_of_previous_cancellations,
  training_set$no_of_previous_cancellations, "3+"
)

training_set$no_of_week_nights <- ifelse(
  training_set$no_of_week_nights %in% top_8_week_nights$no_of_week_nights,
  training_set$no_of_week_nights, "8+"
)

training_set$no_of_weekend_nights <- ifelse(
  training_set$no_of_weekend_nights %in% top_6_weekend_nights$no_of_weekend_nights,
  training_set$no_of_weekend_nights, "6+"
)

training_set$type_of_meal_plan <- ifelse(training_set$type_of_meal_plan %in% c("meal_plan_1", "meal_plan_2"),
                                         training_set$type_of_meal_plan,
                                         "other")

training_set$no_of_special_requests <- ifelse(training_set$no_of_special_requests %in% top_4_special_requests$no_of_special_requests,
                                         training_set$no_of_special_requests,
                                         "4+")

training_set$arrival_date <- parse_date_time(training_set$arrival_date, "ymd")
training_set$booking_date <- int_start(interval(training_set$arrival_date - ddays(training_set$lead_time), 
                                        training_set$arrival_date))

season_months <- data.frame(winter = c(12,1,2), spring = c(3,4,5),
                            summer = c(6,7,8), fall = c(9,10,11))


get_season <- function(x) {
  y <- month(x)
  # print(y)
  for (j in 1:length(colnames(season_months))) {
    # print(j)
    if (y %in% season_months[[j]]) {
      # print(colnames(season_months)[j])
      return(colnames(season_months)[j])
    }
  }
}

training_set$arrival_season <- sapply(training_set$arrival_date, get_season)
training_set$arrival_day <- wday(training_set$arrival_date)
training_set$arrival_day <- ifelse(training_set$arrival_day %in% c(1,6,7),
                                   "weekend", "weekday")


categorical_var <- c(1:7,10:13,15,18,19)
for (i in categorical_var) {
  training_set[[i]] <- factor(training_set[[i]])
}

cat_col <- colnames(training_set[, c(1:7,10:13,15,18,19)])

onehot_encoder <- dummyVars(~ no_of_adults + no_of_children + no_of_weekend_nights
                            + no_of_week_nights + type_of_meal_plan + required_car_parking_space
                            + room_type_reserved + market_segment_type + repeated_guest
                            + no_of_previous_cancellations + no_of_previous_bookings_not_canceled
                            + no_of_special_requests + arrival_season + arrival_day,
                            training_set[, c("no_of_adults","no_of_children","no_of_weekend_nights",
                                             "no_of_week_nights","type_of_meal_plan",
                                             "required_car_parking_space",
                                             "room_type_reserved","market_segment_type",
                                             "repeated_guest","no_of_previous_cancellations",
                                             "no_of_previous_bookings_not_canceled",
                                             "no_of_special_requests","arrival_season",
                                             "arrival_day")],
                            levelsOnly = F,
                            fullRank = T)

onehot_enc_training <- predict(onehot_encoder, training_set[, c("no_of_adults","no_of_children","no_of_weekend_nights",
                                                                "no_of_week_nights","type_of_meal_plan",
                                                                "required_car_parking_space",
                                                                "room_type_reserved","market_segment_type",
                                                                "repeated_guest","no_of_previous_cancellations",
                                                                "no_of_previous_bookings_not_canceled",
                                                                "no_of_special_requests","arrival_season",
                                                                "arrival_day")])

training_set <- cbind(training_set, onehot_enc_training)

####### Test Set Categorical Variables

test_set$no_of_previous_bookings_not_canceled <- ifelse(
  test_set$no_of_previous_bookings_not_canceled %in% top_8_previous_not_cancelled$no_of_previous_bookings_not_canceled,
  test_set$no_of_previous_bookings_not_canceled, "8+"
)

test_set$no_of_children <- ifelse(
  test_set$no_of_children %in% top_2_number_of_children$no_of_children,
  test_set$no_of_children, "3+"
)

test_set$no_of_previous_cancellations <- ifelse(
  test_set$no_of_previous_cancellations %in% top_3_previous_cancellations$no_of_previous_cancellations,
  test_set$no_of_previous_cancellations, "3+"
)

test_set$no_of_week_nights <- ifelse(
  test_set$no_of_week_nights %in% top_8_week_nights$no_of_week_nights,
  test_set$no_of_week_nights, "8+"
)

test_set$no_of_weekend_nights <- ifelse(
  test_set$no_of_weekend_nights %in% top_6_weekend_nights$no_of_weekend_nights,
  test_set$no_of_weekend_nights, "6+"
)

test_set$type_of_meal_plan <- ifelse(test_set$type_of_meal_plan %in% c("meal_plan_1", "meal_plan_2"),
                                         test_set$type_of_meal_plan,
                                         "other")

test_set$no_of_special_requests <- ifelse(test_set$no_of_special_requests %in% top_4_special_requests$no_of_special_requests,
                                              test_set$no_of_special_requests,
                                              "4+")

test_set$arrival_date <- parse_date_time(test_set$arrival_date, "ymd")
test_set$booking_date <- int_start(interval(test_set$arrival_date - ddays(test_set$lead_time), 
                                                test_set$arrival_date))


test_set$arrival_season <- sapply(test_set$arrival_date, get_season)
test_set$arrival_day <- wday(test_set$arrival_date)
test_set$arrival_day <- ifelse(test_set$arrival_day %in% c(1,6,7),
                                   "weekend", "weekday")


categorical_var <- c(1:7,10:13,15,18,19)
for (i in categorical_var) {
  test_set[[i]] <- factor(test_set[[i]])
}

cat_col <- colnames(test_set[, c(1:7,10:13,15,18,19)])

onehot_encoder <- dummyVars(~ no_of_adults + no_of_children + no_of_weekend_nights
                            + no_of_week_nights + type_of_meal_plan + required_car_parking_space
                            + room_type_reserved + market_segment_type + repeated_guest
                            + no_of_previous_cancellations + no_of_previous_bookings_not_canceled
                            + no_of_special_requests + arrival_season + arrival_day,
                            test_set[, c("no_of_adults","no_of_children","no_of_weekend_nights",
                                             "no_of_week_nights","type_of_meal_plan",
                                             "required_car_parking_space",
                                             "room_type_reserved","market_segment_type",
                                             "repeated_guest","no_of_previous_cancellations",
                                             "no_of_previous_bookings_not_canceled",
                                             "no_of_special_requests","arrival_season",
                                             "arrival_day")],
                            levelsOnly = F,
                            fullRank = T)

onehot_enc_test <- predict(onehot_encoder, test_set[, c("no_of_adults","no_of_children","no_of_weekend_nights",
                                                                "no_of_week_nights","type_of_meal_plan",
                                                                "required_car_parking_space",
                                                                "room_type_reserved","market_segment_type",
                                                                "repeated_guest","no_of_previous_cancellations",
                                                                "no_of_previous_bookings_not_canceled",
                                                                "no_of_special_requests","arrival_season",
                                                                "arrival_day")])

test_set <- cbind(test_set, onehot_enc_test)


##### Numerical Features

test_set[, c("lead_time", "avg_price_per_room")] <- scale(test_set[, c("lead_time", "avg_price_per_room")],
                                                          center = apply(training_set[, c("lead_time", "avg_price_per_room")], 2, mean),
                                                          scale = apply(training_set[, c("lead_time", "avg_price_per_room")], 2, sd))
training_set[, c("lead_time", "avg_price_per_room")] <- scale(training_set[, c("lead_time", "avg_price_per_room")])


##### Create tensors

train_col <- ncol(training_set)
test_col <- ncol(test_set)

training_features <- array(data = unlist(training_set[, c(8,14,20:train_col)]),
                           dim = c(nrow(training_set), length(c(8,14,20:train_col))))
training_labels <- array(data = unlist(training_set[, "booking_status"]),
                         dim = nrow(training_set))

test_features <- array(data = unlist(test_set[, c(8,14,20:test_col)]),
                       dim = c(nrow(test_set), length(c(8,14,20:test_col))))
test_labels <- array(data = unlist(test_set[, "booking_status"]),
                     dim = nrow(test_set))

# colnames(training_set)[!(colnames(training_set) %in% colnames(test_set))]

# project_data %>%
  # group_by(no_of_special_requests) %>%
  # summarise(count = n())

##### Dense feed-forward Neural Network

library(reticulate)
library(tensorflow)
library(keras)

use_virtualenv("my_tf_workspace")

# Overfit model

model <- keras_model_sequential(list(
  layer_dense(units = 100, activation = "relu"),
  layer_dense(units = 100, activation = "relu"),
  layer_dense(units = 50, activation = "relu"),
  layer_dense(units = 1, activation = "sigmoid")
))

compile(model,
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = "accuracy")

history <- fit(model, training_features, training_labels,
               epochs = 200, batch_size = 512, validation_split = 0.33)
plot(history)

# Model with smaller capacity

model <- keras_model_sequential(list(
  layer_dense(units = 75, activation = "relu"),
  layer_dense(units = 37, activation = "relu"),
  layer_dense(units = 1, activation = "sigmoid")
))

compile(model,
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = "accuracy")

history <- fit(model, training_features, training_labels,
               epochs = 200, batch_size = 512, validation_split = 0.33)
plot(history)

# Add early stop and batch normalization model
model <- keras_model_sequential(list(
  layer_dense(units = 75, activation = "relu"),
  layer_batch_normalization(),
  layer_dense(units = 37, activation = "relu"),
  layer_batch_normalization(),
  layer_dense(units = 1, activation = "sigmoid")
))

compile(model,
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = "accuracy")

history <- fit(model, training_features, training_labels,
               epochs = 25, batch_size = 512, validation_split = 0.33,
               callbacks = list(callback_early_stopping(patience = 2)))
plot(history)

# Add L2 regularization

model <- keras_model_sequential(list(
  layer_dense(units = 75, activation = "relu",
              kernel_regularizer = regularizer_l2(0.002)),
  layer_batch_normalization(),
  layer_dense(units = 37, activation = "relu",
              kernel_regularizer = regularizer_l2(0.002)),
  layer_batch_normalization(),
  layer_dense(units = 1, activation = "sigmoid")
))

compile(model,
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = "accuracy")

history <- fit(model, training_features, training_labels,
               epochs = 25, batch_size = 512, validation_split = 0.33,
               callbacks = list(callback_early_stopping(patience = 2)))
plot(history)

# Add Dropout

model <- keras_model_sequential(list(
  layer_dense(units = 75, activation = "relu",
              kernel_regularizer = regularizer_l2(0.002)),
  layer_batch_normalization(),
  layer_dropout(rate=0.5),
  layer_dense(units = 37, activation = "relu",
              kernel_regularizer = regularizer_l2(0.002)),
  layer_batch_normalization(),
  layer_dropout(rate=0.5),
  layer_dense(units = 1, activation = "sigmoid")
))

compile(model,
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = "accuracy")

history <- fit(model, training_features, training_labels,
               epochs = 25, batch_size = 512, validation_split = 0.33,
               callbacks = list(callback_early_stopping(patience = 2)))
plot(history)

# Remove regularization

model <- keras_model_sequential(list(
  layer_dense(units = 75, activation = "relu"),
  layer_batch_normalization(),
  layer_dropout(rate=0.5),
  layer_dense(units = 37, activation = "relu"),
  layer_batch_normalization(),
  layer_dropout(rate=0.5),
  layer_dense(units = 1, activation = "sigmoid")
))

compile(model,
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = "accuracy")

history <- fit(model, training_features, training_labels,
               epochs = 25, batch_size = 512, validation_split = 0.33,
               callbacks = list(callback_early_stopping(patience = 2)))
plot(history)

# Add regularization and increase nodes in hidden layers

model <- keras_model_sequential(list(
  layer_dense(units = 100, activation = "relu",
              kernel_regularizer = regularizer_l2(0.002)),
  layer_batch_normalization(),
  layer_dropout(rate=0.5),
  layer_dense(units = 50, activation = "relu",
              kernel_regularizer = regularizer_l2(0.002)),
  layer_batch_normalization(),
  layer_dropout(rate=0.5),
  layer_dense(units = 1, activation = "sigmoid")
))

compile(model,
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = "accuracy")

history <- fit(model, training_features, training_labels,
               epochs = 25, batch_size = 512, validation_split = 0.33,
               callbacks = list(callback_early_stopping(patience = 2)))
plot(history)

# 50 epochs YIELDS LOWEST VALIDATION LOSS

model <- keras_model_sequential(list(
  layer_dense(units = 100, activation = "relu",
              kernel_regularizer = regularizer_l2(0.002)),
  layer_batch_normalization(),
  layer_dropout(rate=0.5),
  layer_dense(units = 50, activation = "relu",
              kernel_regularizer = regularizer_l2(0.002)),
  layer_batch_normalization(),
  layer_dropout(rate=0.5),
  layer_dense(units = 1, activation = "sigmoid")
))

compile(model,
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = "accuracy")

history <- fit(model, training_features, training_labels,
               epochs = 50, batch_size = 512, validation_split = 0.33,
               callbacks = list(callback_early_stopping(patience = 2)))
plot(history)

# Remove dropout

model <- keras_model_sequential(list(
  layer_dense(units = 100, activation = "relu",
              kernel_regularizer = regularizer_l2(0.002)),
  layer_batch_normalization(),
  layer_dense(units = 50, activation = "relu",
              kernel_regularizer = regularizer_l2(0.002)),
  layer_batch_normalization(),
  layer_dense(units = 1, activation = "sigmoid")
))

compile(model,
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = "accuracy")

history <- fit(model, training_features, training_labels,
               epochs = 50, batch_size = 512, validation_split = 0.33,
               callbacks = list(callback_early_stopping(patience = 2)))
plot(history)

########################### Evaluate model #####################################

predictions <- predict(model, test_features)
test_set$p_prob <- predictions[, 1]

# Use 0.5 threshold
over_threshold <- test_set[test_set$p_prob >= 0.5, ] # observations that will cancel

fpr <- sum(over_threshold$booking_status==0)/sum(test_set$booking_status==0)
fpr

tpr <- sum(over_threshold$booking_status==1)/sum(test_set$booking_status==1)
tpr

# Use 0.75 threshold
over_threshold <- test_set[test_set$p_prob >= 0.75, ] # observations that will cancel

fpr <- sum(over_threshold$booking_status==0)/sum(test_set$booking_status==0)
fpr

tpr <- sum(over_threshold$booking_status==1)/sum(test_set$booking_status==1)
tpr

# Use a 0.33 threshold
over_threshold <- test_set[test_set$p_prob >= 0.33, ] # observations that will cancel

fpr <- sum(over_threshold$booking_status==0)/sum(test_set$booking_status==0)
fpr

tpr <- sum(over_threshold$booking_status==1)/sum(test_set$booking_status==1)
tpr

# ROC Curve

roc_data <- data.frame(threshold=seq(1,0,-0.01), fpr=0, tpr=0)
for (i in roc_data$threshold) {
  
  over_threshold <- test_set[test_set$p_prob >= i, ]
  
  fpr <- sum(over_threshold$booking_status==0)/sum(test_set$booking_status==0)
  roc_data[roc_data$threshold==i, "fpr"] <-  fpr
  
  tpr <- sum(over_threshold$booking_status==1)/sum(test_set$booking_status==1)
  roc_data[roc_data$threshold==i, "tpr"] <- tpr
  
}

ggplot() +
  geom_line(data = roc_data, aes(x=fpr, y=tpr, color = threshold), size = 2) +
  scale_color_gradientn(colors = rainbow(3)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_point(data = roc_data[seq(1, 101, 10), ], aes(x = fpr, y =tpr)) +
  geom_text(data = roc_data[seq(1, 101, 10), ],
            aes(x = fpr, y = tpr, label = threshold, hjust = 1.2, vjust = -0.2))

# AUC

library(MESS)

auc <- auc(x = roc_data$fpr, y = roc_data$tpr, type = "spline")
auc

# Calibration Curve

calibration_data <- data.frame(bin_midpoint=seq(0.05, 0.95, 0.1),
                               observed_event_percentage=0)
for (i in seq(0.05,0.95,0.1)) {
  
  in_interval <- test_set[test_set$p_prob >= (i-0.05) & test_set$p_prob <= (i+0.05), ]
  oep <- nrow(in_interval[in_interval$booking_status==1, ])/nrow(in_interval)
  calibration_data[calibration_data$bin_midpoint==i, "observed_event_percentage"] <- oep
  
}

ggplot(data = calibration_data, aes(x = bin_midpoint, y = observed_event_percentage)) +
  geom_line(size = 1) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_point(size = 2) +
  geom_text(aes(label = bin_midpoint), hjust = 0.75, vjust = -0.5)


############################ Smaller capacity model ############################

# Evaluate a model with smaller capacity that does not over fit before generalization

model_small <- keras_model_sequential(list(
  layer_dense(units = 75, activation = "relu"),
  layer_dense(units = 37, activation = "relu"),
  layer_dense(units = 1, activation = "sigmoid")
))

compile(model_small,
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = "accuracy")

history_small <- fit(model_small, training_features, training_labels,
                     epochs = 200, batch_size = 512, validation_split = 0.33)
plot(history_small)

# Generalize the model

model <- keras_model_sequential(list(
  layer_dense(units = 75, activation = "relu",
              kernel_regularizer = regularizer_l2(0.002)),
  layer_batch_normalization(),
  layer_dropout(rate=0.2),
  layer_dense(units = 37, activation = "relu",
              kernel_regularizer = regularizer_l2(0.002)),
  layer_batch_normalization(),
  layer_dropout(rate=0.2),
  layer_dense(units = 1, activation = "sigmoid")
))

compile(model,
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = "accuracy")

history <- fit(model, training_features, training_labels,
               epochs = 50, batch_size = 512, validation_split = 0.33,
               callbacks = list(callback_early_stopping(patience = 2)))
plot(history)

# Evaluate the smaller capacity model

predictions <- predict(model, test_features)
test_set$p_prob2 <- predictions[, 1]

# ROC curve

roc_data <- data.frame(threshold=seq(1,0,-0.01), fpr=0, tpr=0)
for (i in roc_data$threshold) {
  
  over_threshold <- test_set[test_set$p_prob2 >= i, ]
  
  fpr <- sum(over_threshold$booking_status==0)/sum(test_set$booking_status==0)
  roc_data[roc_data$threshold==i, "fpr"] <-  fpr
  
  tpr <- sum(over_threshold$booking_status==1)/sum(test_set$booking_status==1)
  roc_data[roc_data$threshold==i, "tpr"] <- tpr
  
}

ggplot() +
  geom_line(data = roc_data, aes(x=fpr, y=tpr, color = threshold), size = 2) +
  scale_color_gradientn(colors = rainbow(3)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_point(data = roc_data[seq(1, 101, 10), ], aes(x = fpr, y =tpr)) +
  geom_text(data = roc_data[seq(1, 101, 10), ],
            aes(x = fpr, y = tpr, label = threshold, hjust = 1.2, vjust = -0.2))


# AUC

auc <- auc(x = roc_data$fpr, y = roc_data$tpr, type = "spline")
auc

# Calibration curve

calibration_data <- data.frame(bin_midpoint=seq(0.05, 0.95, 0.1),
                               observed_event_percentage=0)
for (i in seq(0.05,0.95,0.1)) {
  
  in_interval <- test_set[test_set$p_prob2 >= (i-0.05) & test_set$p_prob2 <= (i+0.05), ]
  oep <- nrow(in_interval[in_interval$booking_status==1, ])/nrow(in_interval)
  calibration_data[calibration_data$bin_midpoint==i, "observed_event_percentage"] <- oep
  
}

ggplot(data = calibration_data, aes(x = bin_midpoint, y = observed_event_percentage)) +
  geom_line(size = 1) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_point(size = 2) +
  geom_text(aes(label = bin_midpoint), hjust = 0.75, vjust = -0.5)
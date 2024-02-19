

library(dplyr)
library(lubridate)
library(caret)

project_data <- read.csv("project_data/project_data.csv")

rownames(project_data) <- project_data$Booking_ID
project_data <- project_data[, -1]

training_ind <- createDataPartition(project_data$booking_status,
                                    p = 0.75,
                                    list = F,
                                    times = 1)

training_set <- project_data[training_ind, ]
test_set <- project_data[-training_ind, ]


training_set$booking_status <- ifelse(training_set$booking_status=="canceled",1,0)
test_set$booking_status <- ifelse(test_set$booking_status=="canceled",1,0)


###### Training Set Categorical Features


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


##### Dense feed-forward Neural Network

library(reticulate)
library(tensorflow)
library(keras)

use_virtualenv("my_tf_workspace")

# First model
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
               epochs = 100, batch_size = 512, validation_split = 0.33)
plot(history)


history2 <- fit(model, training_features, training_labels,
               epochs = 100, batch_size = 1000, validation_split = 0.33)
plot(history2)


history6 <- fit(model, training_features, training_labels,
                epochs = 200, batch_size = 512, validation_split = 0.33)
plot(history6)

# Second model

model_2 <- keras_model_sequential(list(
  layer_dense(units = 75, activation = "relu"),
  layer_dense(units = 75, activation = "relu"),
  layer_dense(units = 37, activation = "relu"),
  layer_dense(units = 1, activation = "sigmoid")
))

compile(model_2,
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = "accuracy")

history3 <- fit(model_2, training_features, training_labels,
               epochs = 100, batch_size = 1000, validation_split = 0.33)

plot(history3)

history4 <- fit(model_2, training_features, training_labels,
                epochs = 100, batch_size = 512, validation_split = 0.33)

plot(history4)


# Third model

model_3 <- keras_model_sequential(list(
  layer_dense(units = 75, activation = "relu"),
  layer_dense(units = 75, activation = "relu"),
  layer_dense(units = 75, activation = "relu"),
  layer_dense(units = 37, activation = "relu"),
  layer_dense(units = 1, activation = "sigmoid")
))

compile(model_3,
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = "accuracy")

history5 <- fit(model_3, training_features, training_labels,
                epochs = 100, batch_size = 1000, validation_split = 0.33)

plot(history5)


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

training_set$arrival_date <- parse_date_time(training_set$arrival_date, "ymd")
training_set$booking_date <- int_start(interval(training_set$arrival_date - ddays(training_set$lead_time), 
                                        training_set$arrival_date))

season_months <- data.frame(winter = c(12,1,2), spring = c(3,4,5),
                            summer = c(6,7,8), fall = c(9,10,11))


date <- parse_date_time("2024-02-17", "ymd")

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

View(training_set[, c(1:7,10:13,15,18,19)])

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


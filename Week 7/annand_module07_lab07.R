
library(dplyr)
library(caret)
library(keras)
library(reticulate)
library(tensorflow)
library(MESS)

########################### Data Pre-processing ################################

data <- read.csv("lab_7_data/lab_7_data.csv")
training_ind <- createDataPartition(data$lodgepole_pine,
                                    p = 0.75,
                                    list = F,
                                    times = 1)

training_set <- data[training_ind, ]
test_set <- data[-training_ind, ]

top_20_soil_types <- training_set %>%
  group_by(soil_type) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  select(soil_type) %>%
  top_n(20)

training_set$soil_type <- ifelse(training_set$soil_type %in% top_20_soil_types$soil_type,
                                 training_set$soil_type,
                                 "other")

training_set$wilderness_area <- factor(training_set$wilderness_area)
training_set$soil_type <- factor(training_set$soil_type)


onehot_encoder <- dummyVars(~ wilderness_area + soil_type,
                            training_set[, c("wilderness_area", "soil_type")],
                            levelsOnly = T,
                            fullRank = T)

onehot_enc_training <- predict(onehot_encoder,
                               training_set[, c("wilderness_area", "soil_type")])

training_set <- cbind(training_set, onehot_enc_training)


test_set$soil_type <- ifelse(test_set$soil_type %in% top_20_soil_types$soil_type,
                             test_set$soil_type,
                             "other")

test_set$wilderness_area <- factor(test_set$wilderness_area)
test_set$soil_type <- factor(test_set$soil_type)

onehot_enc_test <- predict(onehot_encoder, test_set[, c("wilderness_area", "soil_type")])
test_set <- cbind(test_set, onehot_enc_test)


test_set[, -c(11:13)] <- scale(test_set[, -c(11:13)],
                               center = apply(training_set[, -c(11:13)], 2, mean),
                               scale = apply(training_set[, -c(11:13)], 2, sd))
training_set[, -c(11:13)] <- scale(training_set[, -c(11:13)])


training_features <- array(data = unlist(training_set[, -c(11:13)]),
                           dim = c(nrow(training_set), 33))
training_labels <- array(data = unlist(training_set[, 13]),
                         dim = c(nrow(training_set)))

test_features <- array(data = unlist(training_set[, -c(11:13)]),
                       dim = c(nrow(test_set), 33))
test_labels <- array(data = unlist(training_set[, 13]),
                     dim = c(nrow(test_set)))


####################### Principal Components Analysis ##########################


pca_results <- prcomp(training_features[, 1:10])
summary(pca_results)

screeplot(pca_results, type = "line")


training_rotated <- as.matrix(training_features[, 1:10]) %*% pca_results$rotation
training_features <- cbind(training_features, training_rotated[,1:6])

test_rotated <- as.matrix(test_features[,1:10]) %*% pca_results$rotation
test_features <- as.matrix(test_features, test_rotated[,1:6])


############################## Neural Network ##################################


use_virtualenv("my_tf_workspace")

model <- keras_model_sequential(list(
  layer_dense(units = 50, activation = "relu"),
  layer_dense(units = 25, activation = "relu"),
  layer_dense(units = 1, activation = "sigmoid")
))

compile(model,
        optimizer = "rmsprop",
        loss = "binary_crossentropy",
        metrics = "accuracy")

history <- fit(model, training_features[, -1*c(1:13)], training_labels,
               epoch = 40, batch_size = 512, validation_split = 0.33)

predictions <- predict(model, test_features[, -1*c(1:13)])
test_set$p_prob <- predictions[, 1]


################################## ROC Curve ###################################


roc_data <- data.frame(threshold=seq(1,0,-0.01), fpr=0, tpr=0)
for (i in roc_data$threshold) {
  
  over_threshold <- test_set[test_set$p_prob >= i, ]
  
  fpr <- sum(over_threshold$lodgepole_pine==0)/sum(test_set$lodgepole_pine==0)
  roc_data[roc_data$threshold==i, "fpr"] <-  fpr
  
  tpr <- sum(over_threshold$lodgepole_pine==1)/sum(test_set$lodgepole_pine==1)
  roc_data[roc_data$threshold==i, "tpr"] <- tpr
  
}

# ROC curve plot
ggplot() +
  geom_line(data = roc_data, aes(x=fpr, y=tpr, color = threshold), size = 2) +
  scale_color_gradientn(colors = rainbow(3)) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  geom_point(data = roc_data[seq(1, 101, 10), ], aes(x = fpr, y =tpr)) +
  geom_text(data = roc_data[seq(1, 101, 10), ],
            aes(x = fpr, y = tpr, label = threshold, hjust = 1.2, vjust = -0.2))

# AUC - area under ROC curve 

auc <- auc(x = roc_data$fpr, y = roc_data$tpr, type = "spline")
auc
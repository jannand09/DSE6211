
library(reticulate)
library(keras)
library(tensorflow)

use_virtualenv("my_tf_workspace", required = TRUE)


mtcars <- mtcars

mtcars_x <- mtcars[, c("cyl", "disp", "hp")]
mtcars_x <- array(data = unlist(mtcars_x),
                  dim = c(32, 3),
                  dimnames = list(rownames(mtcars_x),
                                  colnames(mtcars_x)))

mtcars_y <- mtcars[, "mpg"]

nn_model <- keras_model_sequential() %>%
  layer_dense(units = 1, input_shape = 3, activation = "linear")

nn_model
# 4 weights in the output layer: 3 for units of the input layer and 1 for the bias

nn_model <- keras_model_sequential() %>%
  layer_dense(units = 2, input_shape = 3, activation = "relu") %>%
  layer_dense(units = 1, activation = "linear")

nn_model
# 8 weights for the first layer: 6 for the connections between the input layer with 
# 3 units and the hidden layer with 2 units (3x2 = 6) and two for the bias

nn_model %>% compile(optimizer = optimizer_adam(learning_rate = 0.2),
                     loss = "mean_squared_error",
                     metrics = "mean_absolute_error")

nn_model_training <- nn_model %>% fit(x = mtcars_x,
                                      y = mtcars_y,
                                      epochs = 250,
                                      verbose = FALSE)

plot(nn_model_training)

get_weights(nn_model)

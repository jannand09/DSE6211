
install.packages("reticulate")
install.packages("keras")
install.packages("tensorflow")


reticulate::virtualenv_create("my_tf_workspace",
                  python = 'C:\\Users\\janna\\AppData\\Local\\Programs\\Python\\Python311\\python.exe')

reticulate::virtualenv_install(envname = "my_tf_workspace", packages = "tensorflow")
reticulate::use_virtualenv(virtualenv = "my_tf_workspace", required = TRUE)

library(tensorflow)
tf$constant("Hello Tensorflow!")

library(keras)
mtcars <- mtcars

# Create an array with values for cylinders, displacement, and horsepower from
# mtcars dataset
mtcars_x <- mtcars[, c("cyl", "disp", "hp")]
mtcars_x <- array(data = unlist(mtcars_x),
                  dim = c(32, 3),
                  dimnames = list(rownames(mtcars_x),
                                  colnames(mtcars_x)))
# Create a vector with miles per gallon values from mtcars dataset
mtcars_y <- mtcars[, "mpg"]

# Specify architecture of neural network model to predict mpg
nn_model <- keras_model_sequential() %>%
  layer_dense(units = 1, input_shape = 3, activation = "linear")

# Specify optimization algorithm as Adam algorithm, learning rate,
# and loss function as MSE 
nn_model %>% compile(optimizer = optimizer_adam(learning_rate = 0.2),
                     loss = "mean_squared_error")

# Fit the neural network using the 
nn_model_training <- nn_model %>% fit(x = mtcars_x,
                                      y = mtcars_y,
                                      epoch = 10000,
                                      verbose = FALSE)

get_weights(nn_model)

lr_fit <- lm(mpg ~ cyl + disp + hp, data = mtcars)
lr_fit$coefficients

# https://stackoverflow.com/questions/59986841/requested-version-of-python-cannot-be-used-as-another-version-has-been-initializ
# https://stackoverflow.com/questions/647515/how-can-i-find-where-python-is-installed-on-windows
# https://devblogs.microsoft.com/scripting/table-of-basic-powershell-commands/

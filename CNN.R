# Install and load required libraries
if (!requireNamespace("keras", quietly = TRUE)) install.packages("keras")
if (!requireNamespace("tensorflow", quietly = TRUE)) install.packages("tensorflow")
library(keras)
install_keras()

# Load the MNIST dataset
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# Reshape and normalize the data
x_train <- array_reshape(x_train, c(nrow(x_train), 28, 28, 1)) / 255
x_test <- array_reshape(x_test, c(nrow(x_test), 28, 28, 1)) / 255

# Convert labels to one-hot encoding
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

# Function to build a CNN model (configurable for dropout)
build_model <- function(use_dropout = FALSE) {
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu", input_shape = c(28, 28, 1)) %>%
    layer_max_pooling_2d(pool_size = c(2, 2))
  
  if (use_dropout) model <- model %>% layer_dropout(rate = 0.25)
  
  model <- model %>%
    layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
    layer_max_pooling_2d(pool_size = c(2, 2))
  
  if (use_dropout) model <- model %>% layer_dropout(rate = 0.25)
  
  model <- model %>%
    layer_flatten() %>%
    layer_dense(units = 128, activation = "relu")
  
  if (use_dropout) model <- model %>% layer_dropout(rate = 0.5)
  
  model <- model %>%
    layer_dense(units = 10, activation = "softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(),
    metrics = c("accuracy")
  )
  
  return(model)
}

# Build and train a basic CNN model
basic_model <- build_model(use_dropout = FALSE)
history_basic <- basic_model %>% fit(
  x_train, y_train,
  epochs = 5,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate the basic model
cat("Basic CNN Model Results:\n")
basic_score <- basic_model %>% evaluate(x_test, y_test)
cat("Test Loss:", basic_score$loss, "\n")
cat("Test Accuracy:", basic_score$accuracy, "\n\n")

# Build and train a CNN model with dropout
dropout_model <- build_model(use_dropout = TRUE)
history_dropout <- dropout_model %>% fit(
  x_train, y_train,
  epochs = 5,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate the dropout model
cat("CNN Model with Dropout Results:\n")
dropout_score <- dropout_model %>% evaluate(x_test, y_test)
cat("Test Loss:", dropout_score$loss, "\n")
cat("Test Accuracy:", dropout_score$accuracy, "\n\n")

# Data augmentation setup
data_generator <- image_data_generator(
  rotation_range = 10,
  width_shift_range = 0.1,
  height_shift_range = 0.1,
  zoom_range = 0.1
)
augmented_train <- flow_images_from_data(x_train, y_train, generator = data_generator, batch_size = 32)

# Train the basic model with augmented data
history_augmented <- basic_model %>% fit_generator(
  augmented_train,
  steps_per_epoch = 500,   # Number of batches per epoch
  epochs = 5,
  validation_data = list(x_test, y_test)
)

# Evaluate the model trained with augmented data
cat("CNN Model with Data Augmentation Results:\n")
augmented_score <- basic_model %>% evaluate(x_test, y_test)
cat("Test Loss:", augmented_score$loss, "\n")
cat("Test Accuracy:", augmented_score$accuracy, "\n\n")

# Visualize training results
par(mfrow = c(1, 3))
plot(history_basic, main = "Basic CNN")
plot(history_dropout, main = "CNN with Dropout")
plot(history_augmented, main = "CNN with Data Augmentation")


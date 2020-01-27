library(tidyverse)
library(keras)
library(readr)
library(h5)

set.seed(123) 
n_sample <- 5000 
max_features <- 5000 
maxlen <- 300


imdb <- dataset_imdb(num_words = max_features)
c(c(x_train, y_train), c(x_test, y_test)) %<-% imdb # Loads the data

x_train <- pad_sequences(x_train, maxlen = maxlen) 
x_test <- pad_sequences(x_test, maxlen = maxlen)

sample_indicators = sample(1:nrow(x_train), n_sample)

x_train <- x_train[sample_indicators,] # use a subset of reviews for training 
y_train <- y_train[sample_indicators] # use a subset of reviews for training
x_test <- x_test[sample_indicators,] # use a subset of reviews for testing 
y_test <- y_test[sample_indicators] # use a subset of reviews for testing


write_rds(x_test, "x_test.rds")
write_rds(y_test, "y_test.rds")

##-------------------------------------------------------------------
#Simple RNN model
rnn_model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 32) %>% 
  layer_simple_rnn(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")

rnn_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy", 
  metrics = c("acc")
)

rnn_history <- rnn_model %>% fit(
  x_train, y_train, 
  epochs = 10,
  batch_size = 128, 
  validation_split = 0.2
)

save_model_hdf5(rnn_model, "rnn_model.h5", 
                overwrite = TRUE,
                include_optimizer = TRUE)

write_rds(rnn_history, "rnn_history.rds")

plot(rnn_history)

##-------------------------------------------------------------------
#LSTM model
lstm_model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 32) %>% 
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")

lstm_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy", 
  metrics = c("acc")
)

lstm_history <- lstm_model %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 128,
  validation_split = 0.2
)
save_model_hdf5(lstm_model, "lstm_model.h5", 
                overwrite = TRUE,
                include_optimizer = TRUE)
write_rds(lstm_history, "lstm_history.rds")
plot(lstm_history)

##-------------------------------------------------------------------
#Bidirectional LSTM model
bi_lstm <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 32) %>%
  bidirectional(
    layer_lstm(units = 32)
  ) %>%               
  layer_dense(units = 1, activation = "sigmoid")

bi_lstm %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy", 
  metrics = c("acc")
)

bi_lstm_history <- bi_lstm %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 128, 
  validation_split = 0.2
)

save_model_hdf5(bi_lstm, "bi_lstm_model.h5", 
                overwrite = TRUE,
                include_optimizer = TRUE)

write_rds(bi_lstm_history, "bi_lstm_history.rds")

plot(bi_lstm_history)

##-------------------------------------------------------------------
#Gated Recurrent Unit model
gru_model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 32) %>% 
  layer_gru(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")

gru_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy", 
  metrics = c("acc")
)

gru_history <- gru_model %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 128,
  validation_split = 0.2
)

save_model_hdf5(gru_model, "gru_model.h5", 
                overwrite = TRUE,
                include_optimizer = TRUE)

write_rds(gru_history, "gru_history.rds")

plot(gru_history)

##-------------------------------------------------------------------
#Bidirectional Gated Recurrent Unit model
bi_gru_model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_features, output_dim = 32) %>% 
  bidirectional(
    layer_gru(units = 32) 
  ) %>%
  layer_dense(units = 1, activation = "sigmoid")

bi_gru_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy", 
  metrics = c("acc")
)

bi_gru_history <- bi_gru_model %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 128,
  validation_split = 0.2
)

write_rds(bi_gru_history, "bi_gru_history.rds")

save_model_hdf5(bi_gru_model, "bi_gru_model.h5", 
                overwrite = TRUE,
                include_optimizer = TRUE)

plot(bi_gru_history)

##-------------------------------------------------------------------
#1D Convnet
conv_1d <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_features, output_dim = 128,
                  input_length = maxlen) %>%
  layer_conv_1d(filters = 32, kernel_size = 7, activation = "relu") %>% 
  layer_max_pooling_1d(pool_size = 5) %>%
  layer_conv_1d(filters = 32, kernel_size = 7, activation = "relu") %>% 
  layer_global_max_pooling_1d() %>%
  layer_dense(units = 1)

summary(conv_1d)

conv_1d %>% compile(
  optimizer = optimizer_rmsprop(lr = 1e-4), 
  loss = "binary_crossentropy",
  metrics = c("acc")
)

conv_1d_history <- conv_1d %>% fit(
  x_train, y_train, 
  epochs = 10,
  batch_size = 128, 
  validation_split = 0.2
)

write_rds(conv_1d_history, "conv_1d_history.rds")

save_model_hdf5(conv_1d, "conv_1d_model.h5", 
                overwrite = TRUE,
                include_optimizer = TRUE)

plot(conv_1d_history)

##-------------------------------------------------------------------
#Combine CNN and RNN
cnn_rnn <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_features, 
                  output_dim = 128,
                  input_length = maxlen) %>%
  layer_conv_1d(filters = 32, kernel_size = 5, 
                activation = "relu",
                input_shape = list(NULL, dim(data)[[-1]])) %>% 
  layer_max_pooling_1d(pool_size = 3) %>%
  layer_conv_1d(filters = 32, kernel_size = 5, activation = "relu") %>% 
  layer_lstm(units = 32) %>%
  layer_dense(units = 1)

summary(cnn_rnn)

cnn_rnn %>% compile(
  optimizer = optimizer_rmsprop(lr = 1e-4), 
  loss = "binary_crossentropy",
  metrics = c("acc")
)

cnn_rnn_history <- cnn_rnn %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 128, 
  validation_split = 0.2
)

write_rds(cnn_rnn_history, "cnn_rnn_history.rds")

save_model_hdf5(cnn_rnn, "cnn_rnn_model.h5", 
                overwrite = TRUE,
                include_optimizer = TRUE)

plot(cnn_rnn_history)
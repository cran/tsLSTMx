#' Check and Format Data
#'
#' This function checks the compatibility of a given data frame and performs necessary formatting.
#'
#' @param data A data frame containing a 'Date' column and a numeric column 'A'.
#' @param n.head Number of rows to display from the formatted data frame (default is 6).
#'
#' @return A formatted data frame with the specified number of rows displayed.
#'
#' @details
#' This function checks the format of the 'Date' column and ensures it is in the format 'dd-mm-yy'.
#' It also checks the presence of the 'A' column and ensures it contains numeric values.
#'
#' @examples
#' \donttest{
#' data <- data.frame(
#'   Date = as.Date(c("01-04-18", "02-04-18", "03-04-18", "04-04-18", "05-04-18",
#'                    "06-04-18", "07-04-18", "08-04-18", "09-04-18", "10-04-18",
#'                    "11-04-18", "12-04-18", "13-04-18", "14-04-18", "15-04-18",
#'                    "16-04-18", "17-04-18", "18-04-18", "19-04-18", "20-04-18"),
#'                  format = "%d-%m-%y"),
#'   A = c(0, 0, 4, 12, 20, 16, 16, 0, 12, 18, 12, 18, 18, 0, 0, 33, 31, 38, 76, 198)
#' )
#' check_and_format_data(data)
#' # Add a new column 'X' based on the values in the second column
#' data$X <- ifelse(data$A != 0, 1, 0)
#' }
#' @importFrom stats embed predict
#' @importFrom utils head
#'
#' @export
check_and_format_data <- function(data, n.head = 6) {
  # Check if the 'Date' column is in the correct format
  if (!inherits(data$Date, "Date") && all(grepl("^\\d{2}-\\d{2}-\\d{2}$", data$Date))) {
    # Convert 'Date' column to Date format
    data$Date <- as.Date(data$Date, format = "%d-%m-%y")
    warning("Dates in the 'Date' column have been successfully reformatted.\n")
  } else if (!inherits(data$Date, "Date")) {
    warning("Warning: 'Date' column format is not recognized. Please make sure it is in the format 'dd-mm-yy'. Attempting to parse...\n")
    # Try to parse 'Date' column
    data$Date <- as.Date(data$Date, format = "%d-%m-%y")
    if (any(is.na(data$Date))) {
      stop("Error: Unable to parse 'Date' column. Please ensure it is in the format 'dd-mm-yy'.")
    } else {
      warning("Dates in the 'Date' column have been successfully reformatted.\n")
    }
  }

  # Check if the 'A' column exists
  if (!"A" %in% colnames(data)) {
    stop("Error: 'A' column not found in the data set. Please make sure it contains the required column 'A'.")
  }

  # Check if the 'A' column has numeric values
  if (!all(is.numeric(data$A))) {
    stop("Error: 'A' column should contain numeric values.")
  }

  warning("Data set compatibility check passed successfully.\n")
  return(head(data, n.head))
}
#' Embed columns and create a new data frame
#'
#' This function takes a data frame and embeds specified columns to create a new data frame.
#'
#' @param data A data frame containing the original columns.
#' @param n_lag Number of lags for embedding.
#'
#' @return A list containing the new data frame and column names of the embedded columns.
#'
#' @examples
#' \donttest{
#' data <- data.frame(
#'   Date = as.Date(c("01-04-18", "02-04-18", "03-04-18", "04-04-18", "05-04-18",
#'                    "06-04-18", "07-04-18", "08-04-18", "09-04-18", "10-04-18",
#'                    "11-04-18", "12-04-18", "13-04-18", "14-04-18", "15-04-18",
#'                    "16-04-18", "17-04-18", "18-04-18", "19-04-18", "20-04-18"),
#'                  format = "%d-%m-%y"),
#'   A = c(0, 0, 4, 12, 20, 16, 16, 0, 12, 18, 12, 18, 18, 0, 0, 33, 31, 38, 76, 198)
#' )
#' check_and_format_data(data)
#' # Add a new column 'X' based on the values in the second column
#' data$X <- ifelse(data$A != 0, 1, 0)
#'
#' result_embed <- embed_columns(data = data, n_lag = 2)
#' new_data <- result_embed$data_frame
#' embedded_colnames <- result_embed$column_names
#' }
#'
#' @export
embed_columns <- function(data, n_lag = 2) {
  A_embed <- embed(data$A, n_lag)
  X_embed <- embed(data$X, n_lag)
  embedded_colnames <- c("y", paste0("y", 1:(n_lag - 1)), "x")
  new_data <- data.frame(A_embed, X_embed[, 1])
  colnames(new_data) <- embedded_colnames
  return(list(data_frame = new_data, column_names = embedded_colnames))
}

#' Split data into training and validation sets
#'
#' This function takes a data frame and splits it into training and validation sets.
#'
#' @param new_data The data frame to be split.
#' @param val_ratio The ratio of the data to be used for validation (default is 0.1).
#'
#' @return A list containing the training and validation data frames.
#'
#' @examples
#' \donttest{
#' data <- data.frame(
#'   Date = as.Date(c("01-04-18", "02-04-18", "03-04-18", "04-04-18", "05-04-18",
#'                    "06-04-18", "07-04-18", "08-04-18", "09-04-18", "10-04-18",
#'                    "11-04-18", "12-04-18", "13-04-18", "14-04-18", "15-04-18",
#'                    "16-04-18", "17-04-18", "18-04-18", "19-04-18", "20-04-18"),
#'                  format = "%d-%m-%y"),
#'   A = c(0, 0, 4, 12, 20, 16, 16, 0, 12, 18, 12, 18, 18, 0, 0, 33, 31, 38, 76, 198)
#' )
#' check_and_format_data(data)
#' # Add a new column 'X' based on the values in the second column
#' data$X <- ifelse(data$A != 0, 1, 0)
#'
#' result_embed <- embed_columns(data = data, n_lag = 2)
#' new_data <- result_embed$data_frame
#' embedded_colnames <- result_embed$column_names
#'
#' result_split <- split_data(new_data = new_data, val_ratio = 0.1)
#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data
#' }
#'
#' @export
split_data <- function(new_data, val_ratio = 0.1) {
  validation_size <- ceiling(val_ratio * nrow(new_data))
  train_data <- new_data[1:(nrow(new_data) - validation_size), ]
  validation_data <- new_data[(nrow(new_data) - validation_size + 1):nrow(new_data), ]
  return(list(train_data = train_data, validation_data = validation_data))
}

#' Function to initialize TensorFlow and enable eager execution
#'
#' This function initializes TensorFlow and enables eager execution.
#'
#' @return No return value, called for smooth running
#'
#' @import tensorflow
#' @importFrom tensorflow tf
#'
#' @examples
#' \donttest{
#' initialize_tensorflow()
#' }
#'
#' @export
initialize_tensorflow <- function() {
  tf$compat$v1$enable_eager_execution()
}

#' Function to convert columns to numeric matrices
#'
#' This function converts specific columns in the data frames to numeric matrices.
#'
#' @param train_data Training data frame.
#' @param validation_data Validation data frame.
#' @param embedded_colnames Names of the embedded columns.
#'
#' @return A list containing numeric matrices for training and validation sets.
#'
#' @examples
#' \donttest{
#' data <- data.frame(
#'   Date = as.Date(c("01-04-18", "02-04-18", "03-04-18", "04-04-18", "05-04-18",
#'                    "06-04-18", "07-04-18", "08-04-18", "09-04-18", "10-04-18",
#'                    "11-04-18", "12-04-18", "13-04-18", "14-04-18", "15-04-18",
#'                    "16-04-18", "17-04-18", "18-04-18", "19-04-18", "20-04-18"),
#'                  format = "%d-%m-%y"),
#'   A = c(0, 0, 4, 12, 20, 16, 16, 0, 12, 18, 12, 18, 18, 0, 0, 33, 31, 38, 76, 198)
#' )
#' check_and_format_data(data)
#' # Add a new column 'X' based on the values in the second column
#' data$X <- ifelse(data$A != 0, 1, 0)
#'
#' result_embed <- embed_columns(data = data, n_lag = 2)
#' new_data <- result_embed$data_frame
#' embedded_colnames <- result_embed$column_names
#'
#' result_split <- split_data(new_data = new_data, val_ratio = 0.1)
#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data

#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data
#' embedded_colnames <- result_embed$column_names
#' numeric_matrices <- convert_to_numeric_matrices(train_data = train_data,
#'                                                 validation_data = validation_data,
#'                                                 embedded_colnames = embedded_colnames)
#' X_train <- numeric_matrices$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- numeric_matrices$X_val
#' y_val <- numeric_matrices$y_val
#'
#' #' initialize_tensorflow()
#' }
#'
#' @export
convert_to_numeric_matrices <- function(train_data, validation_data, embedded_colnames) {
  X_train <- as.matrix(train_data[, embedded_colnames])
  y_train <- as.numeric(train_data$y)
  X_val <- as.matrix(validation_data[, embedded_colnames])
  y_val <- as.numeric(validation_data$y)
  return(list(X_train = X_train, y_train = y_train, X_val = X_val, y_val = y_val))
}

#' Function to reshape input data for LSTM
#'
#' This function reshapes input data to be compatible with LSTM models.
#'
#' @param X_train Numeric matrix representing the training input data.
#' @param X_val Numeric matrix representing the validation input data.
#'
#' @return A list containing reshaped training and validation input data.
#'
#' @import tensorflow
#' @importFrom tensorflow array_reshape
#' @importFrom reticulate import
#' @examples
#' \donttest{
#' data <- data.frame(
#'   Date = as.Date(c("01-04-18", "02-04-18", "03-04-18", "04-04-18", "05-04-18",
#'                    "06-04-18", "07-04-18", "08-04-18", "09-04-18", "10-04-18",
#'                    "11-04-18", "12-04-18", "13-04-18", "14-04-18", "15-04-18",
#'                    "16-04-18", "17-04-18", "18-04-18", "19-04-18", "20-04-18"),
#'                  format = "%d-%m-%y"),
#'   A = c(0, 0, 4, 12, 20, 16, 16, 0, 12, 18, 12, 18, 18, 0, 0, 33, 31, 38, 76, 198)
#' )
#' check_and_format_data(data)
#' # Add a new column 'X' based on the values in the second column
#' data$X <- ifelse(data$A != 0, 1, 0)
#'
#' result_embed <- embed_columns(data = data, n_lag = 2)
#' new_data <- result_embed$data_frame
#' embedded_colnames <- result_embed$column_names
#'
#' result_split <- split_data(new_data = new_data, val_ratio = 0.1)
#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data

#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data
#' embedded_colnames <- result_embed$column_names
#' numeric_matrices <- convert_to_numeric_matrices(train_data = train_data,
#'                                                 validation_data = validation_data,
#'                                                 embedded_colnames = embedded_colnames)
#' X_train <- numeric_matrices$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- numeric_matrices$X_val
#' y_val <- numeric_matrices$y_val
#'
#' #' initialize_tensorflow()
#'
#' X_train <- numeric_matrices$X_train
#' X_val <- numeric_matrices$X_val
#' reshaped_data <- reshape_for_lstm(X_train = X_train, X_val = X_val)
#' X_train <- reshaped_data$X_train
#' X_val <- reshaped_data$X_val
#' }
#'
#' @export
reshape_for_lstm <- function(X_train, X_val) {
  X_train <- array_reshape(X_train, c(dim(X_train)[1], 1, dim(X_train)[2]))
  X_val <- array_reshape(X_val, c(dim(X_val)[1], 1, dim(X_val)[2]))
  return(list(X_train = X_train, X_val = X_val))
}

#' Function to convert data to TensorFlow tensors
#'
#' This function converts input data to TensorFlow tensors for compatibility with TensorFlow and keras models.
#'
#' @param X_train Numeric matrix representing the training input data.
#' @param y_train Numeric vector representing the training output data.
#' @param X_val Numeric matrix representing the validation input data.
#' @param y_val Numeric vector representing the validation output data.
#'
#' @return A list containing TensorFlow tensors for training and validation data.
#'
#' @import tensorflow
#' @examples
#' \donttest{
#' data <- data.frame(
#'   Date = as.Date(c("01-04-18", "02-04-18", "03-04-18", "04-04-18", "05-04-18",
#'                    "06-04-18", "07-04-18", "08-04-18", "09-04-18", "10-04-18",
#'                    "11-04-18", "12-04-18", "13-04-18", "14-04-18", "15-04-18",
#'                    "16-04-18", "17-04-18", "18-04-18", "19-04-18", "20-04-18"),
#'                  format = "%d-%m-%y"),
#'   A = c(0, 0, 4, 12, 20, 16, 16, 0, 12, 18, 12, 18, 18, 0, 0, 33, 31, 38, 76, 198)
#' )
#' check_and_format_data(data)
#' # Add a new column 'X' based on the values in the second column
#' data$X <- ifelse(data$A != 0, 1, 0)
#'
#' result_embed <- embed_columns(data = data, n_lag = 2)
#' new_data <- result_embed$data_frame
#' embedded_colnames <- result_embed$column_names
#'
#' result_split <- split_data(new_data = new_data, val_ratio = 0.1)
#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data

#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data
#' embedded_colnames <- result_embed$column_names
#' numeric_matrices <- convert_to_numeric_matrices(train_data = train_data,
#'                                                 validation_data = validation_data,
#'                                                 embedded_colnames = embedded_colnames)
#' X_train <- numeric_matrices$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- numeric_matrices$X_val
#' y_val <- numeric_matrices$y_val
#'
#' #' initialize_tensorflow()
#'
#' X_train <- numeric_matrices$X_train
#' X_val <- numeric_matrices$X_val
#' reshaped_data <- reshape_for_lstm(X_train = X_train, X_val = X_val)
#' X_train <- reshaped_data$X_train
#' X_val <- reshaped_data$X_val
#' X_train <- reshaped_data$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- reshaped_data$X_val
#' y_val <- numeric_matrices$y_val
#' tf <- reticulate::import("tensorflow")
#' tensors <- convert_to_tensors(X_train = X_train, y_train = y_train, X_val = X_val, y_val = y_val)
#' X_train <- tensors$X_train
#' y_train <- tensors$y_train
#' X_val <- tensors$X_val
#' y_val <- tensors$y_val
#' }
#'
#' @export
convert_to_tensors <- function(X_train, y_train, X_val, y_val) {
  X_train <- tf$constant(X_train)
  y_train <- tf$constant(y_train)
  X_val <- tf$constant(X_val)
  y_val <- tf$constant(y_val)
  return(list(X_train = X_train, y_train = y_train, X_val = X_val, y_val = y_val))
}
#' Function to define early stopping callback
#'
#' This function defines an early stopping callback for keras models.
#'
#' @param n_patience Integer specifying the number of epochs with no improvement after which training will be stopped.
#'
#' @return A keras early stopping callback.
#'
#' @import tensorflow
#' @import keras
#' @importFrom keras callback_early_stopping
#'
#' @examples
#' \donttest{
#' data <- data.frame(
#'   Date = as.Date(c("01-04-18", "02-04-18", "03-04-18", "04-04-18", "05-04-18",
#'                    "06-04-18", "07-04-18", "08-04-18", "09-04-18", "10-04-18",
#'                    "11-04-18", "12-04-18", "13-04-18", "14-04-18", "15-04-18",
#'                    "16-04-18", "17-04-18", "18-04-18", "19-04-18", "20-04-18"),
#'                  format = "%d-%m-%y"),
#'   A = c(0, 0, 4, 12, 20, 16, 16, 0, 12, 18, 12, 18, 18, 0, 0, 33, 31, 38, 76, 198)
#' )
#' check_and_format_data(data)
#' # Add a new column 'X' based on the values in the second column
#' data$X <- ifelse(data$A != 0, 1, 0)
#'
#' result_embed <- embed_columns(data = data, n_lag = 2)
#' new_data <- result_embed$data_frame
#' embedded_colnames <- result_embed$column_names
#'
#' result_split <- split_data(new_data = new_data, val_ratio = 0.1)
#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data

#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data
#' embedded_colnames <- result_embed$column_names
#' numeric_matrices <- convert_to_numeric_matrices(train_data = train_data,
#'                                                 validation_data = validation_data,
#'                                                 embedded_colnames = embedded_colnames)
#' X_train <- numeric_matrices$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- numeric_matrices$X_val
#' y_val <- numeric_matrices$y_val
#'
#' #' initialize_tensorflow()
#'
#' X_train <- numeric_matrices$X_train
#' X_val <- numeric_matrices$X_val
#' reshaped_data <- reshape_for_lstm(X_train = X_train, X_val = X_val)
#' X_train <- reshaped_data$X_train
#' X_val <- reshaped_data$X_val
#' X_train <- reshaped_data$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- reshaped_data$X_val
#' y_val <- numeric_matrices$y_val
#' tf <- reticulate::import("tensorflow")
#' tensors <- convert_to_tensors(X_train = X_train, y_train = y_train, X_val = X_val, y_val = y_val)
#' X_train <- tensors$X_train
#' y_train <- tensors$y_train
#' X_val <- tensors$X_val
#' y_val <- tensors$y_val
#' n_patience <- 50
#' early_stopping <- define_early_stopping(n_patience = n_patience)
#' }
#'
#' @export
define_early_stopping <- function(n_patience) {
  early_stopping <- callback_early_stopping(
    monitor = "val_loss",
    patience = n_patience,
    restore_best_weights = TRUE
  )
  return(early_stopping)
}

#' Time Series LSTM Hyperparameter Tuning
#'
#' This function performs hyperparameter tuning for a Time Series LSTM model using a grid search approach.
#'
#' @param X_train Numeric matrix, the training input data.
#' @param y_train Numeric vector, the training target data.
#' @param X_val Numeric matrix, the validation input data.
#' @param y_val Numeric vector, the validation target data.
#' @param embedded_colnames Character vector, column names of the embedded features.
#' @param custom_loss Function, custom loss function for the LSTM model.
#' @param early_stopping keras early stopping callback.
#' @param n_lag Integer, desired lag value.
#' @param lstm_units_list Numeric vector, list of LSTM units to search over.
#' @param learning_rate_list Numeric vector, list of learning rates to search over.
#' @param batch_size_list Numeric vector, list of batch sizes to search over.
#' @param dropout_list Numeric vector, list of dropout rates to search over.
#' @param l1_reg_list Numeric vector, list of L1 regularization values to search over.
#' @param l2_reg_list Numeric vector, list of L2 regularization values to search over.
#' @param n_iter Integer, number of epochs for each model training.
#' @param n_verbose Integer, level of verbosity during training (0 or 1).
#'
#' @return A list containing the results data frame, all histories, and LSTM models.
#'
#' @examples
#' \donttest{
#' data <- data.frame(
#'   Date = as.Date(c("01-04-18", "02-04-18", "03-04-18", "04-04-18", "05-04-18",
#'                    "06-04-18", "07-04-18", "08-04-18", "09-04-18", "10-04-18",
#'                    "11-04-18", "12-04-18", "13-04-18", "14-04-18", "15-04-18",
#'                    "16-04-18", "17-04-18", "18-04-18", "19-04-18", "20-04-18"),
#'                  format = "%d-%m-%y"),
#'   A = c(0, 0, 4, 12, 20, 16, 16, 0, 12, 18, 12, 18, 18, 0, 0, 33, 31, 38, 76, 198)
#' )
#' check_and_format_data(data)
#' # Add a new column 'X' based on the values in the second column
#' data$X <- ifelse(data$A != 0, 1, 0)
#'
#' result_embed <- embed_columns(data = data, n_lag = 2)
#' new_data <- result_embed$data_frame
#' embedded_colnames <- result_embed$column_names
#'
#' result_split <- split_data(new_data = new_data, val_ratio = 0.1)
#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data

#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data
#' embedded_colnames <- result_embed$column_names
#' numeric_matrices <- convert_to_numeric_matrices(train_data = train_data,
#'                                                 validation_data = validation_data,
#'                                                 embedded_colnames = embedded_colnames)
#' X_train <- numeric_matrices$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- numeric_matrices$X_val
#' y_val <- numeric_matrices$y_val
#'
#' #' initialize_tensorflow()
#'
#' X_train <- numeric_matrices$X_train
#' X_val <- numeric_matrices$X_val
#' reshaped_data <- reshape_for_lstm(X_train = X_train, X_val = X_val)
#' X_train <- reshaped_data$X_train
#' X_val <- reshaped_data$X_val
#' X_train <- reshaped_data$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- reshaped_data$X_val
#' y_val <- numeric_matrices$y_val
#' tf <- reticulate::import("tensorflow")
#' tensors <- convert_to_tensors(X_train = X_train, y_train = y_train, X_val = X_val, y_val = y_val)
#' X_train <- tensors$X_train
#' y_train <- tensors$y_train
#' X_val <- tensors$X_val
#' y_val <- tensors$y_val
#' n_patience <- 50
#' early_stopping <- define_early_stopping(n_patience = n_patience)
#'
#' X_train <- tensors$X_train
#' X_val <- tensors$X_val
#'
#' y_train <- tensors$y_train
#' y_val <- tensors$y_val
#'
#' embedded_colnames <- result_embed$column_names
#'
#' # Define your custom loss function
#' custom_loss <- function(y_true, y_pred) {
#'   condition <- tf$math$equal(y_true, 0)
#'   loss <- tf$math$reduce_mean(tf$math$square(y_true - y_pred))  # Remove 'axis'
#'   loss <- tf$where(condition, tf$constant(0), loss)
#'   return(loss)
#' }
#'
#' early_stopping <- define_early_stopping(n_patience = n_patience)
#'
#' grid_search_results <- ts_lstm_x_tuning(
#'   X_train, y_train, X_val, y_val,
#'   embedded_colnames, custom_loss, early_stopping,
#'   n_lag = 2, # desired lag value
#'   lstm_units_list = c(32),
#'   learning_rate_list = c(0.001, 0.01),
#'   batch_size_list = c(32),
#'   dropout_list = c(0.2),
#'   l1_reg_list = c(0.001),
#'   l2_reg_list = c(0.001),
#'   n_iter = 10,
#'   n_verbose = 0 # or 1
#' )
#'
#' results_df <- grid_search_results$results_df
#' all_histories <- grid_search_results$all_histories
#' lstm_models <- grid_search_results$lstm_models
#'
#' # Find the row with the minimum val_loss_mae in results_df
#' min_val_loss_row <- results_df[which.min(results_df$val_loss_mae), ]
#'
#' # Extract hyperparameters from the row
#' best_lstm_units <- min_val_loss_row$lstm_units
#' best_learning_rate <- min_val_loss_row$learning_rate
#' best_batch_size <- min_val_loss_row$batch_size
#' best_n_lag <- min_val_loss_row$n_lag
#' best_dropout <- min_val_loss_row$dropout
#' best_l1_reg <- min_val_loss_row$l1_reg
#' best_l2_reg <- min_val_loss_row$l2_reg
#'
#' # Generate the lstm_model_name for the best model
#' best_model_name <- paste0("lstm_model_lu_", best_lstm_units, "_lr_", best_learning_rate,
#'                           "_bs_", best_batch_size, "_lag_", best_n_lag,
#'                           "_do_", best_dropout, "_l1_", best_l1_reg, "_l2_", best_l2_reg)
#'
#' # Generate the history_name for the best model
#' best_history_name <- paste0("history_lu_", best_lstm_units, "_lr_", best_learning_rate,
#'                             "_bs_", best_batch_size, "_lag_", best_n_lag,
#'                             "_do_", best_dropout, "_l1_", best_l1_reg, "_l2_", best_l2_reg)
#'
#' # Access the best model from lstm_models
#' best_model <- lstm_models[[best_model_name]]
#'
#' best_model_details <- data.frame(min_val_loss_row)
#'
#' colnames(best_model_details) <- colnames(results_df)
#'
#' # Access the best model from lstm_models
#' best_history <- all_histories[[best_history_name]]
#'}
#'
#' @import tensorflow
#' @import keras
#' @importFrom keras layer_lstm layer_dense optimizer_adam regularizer_l1_l2
#' @references Garai, S., & Paul, R. K. (2023). Development of MCS based-ensemble models using CEEMDAN decomposition and machine intelligence. Intelligent Systems with Applications, 18, 200202.
#' @export
ts_lstm_x_tuning <- function(
    X_train, y_train, X_val, y_val,
    embedded_colnames, custom_loss, early_stopping,
    n_lag = 2,
    lstm_units_list = c(32),
    learning_rate_list = c(0.001, 0.01),
    batch_size_list = c(32),
    dropout_list = c(0.2),
    l1_reg_list = c(0.001),
    l2_reg_list = c(0.001),
    n_iter = 10,
    n_verbose = 0
) {
  results_df <- data.frame(
    lstm_units = numeric(),
    learning_rate = numeric(),
    batch_size = numeric(),
    n_lag = numeric(),
    dropout = numeric(),
    l1_reg = numeric(),
    l2_reg = numeric(),
    training_loss_mae = numeric(),
    val_loss_mae = numeric(),
    early_stopping = numeric()
  )

  all_histories <- list()
  lstm_models <- list()

  for (lstm_units in lstm_units_list) {
    for (learning_rate in learning_rate_list) {
      for (batch_size in batch_size_list) {
        for (dropout in dropout_list) {
          for (l1_reg in l1_reg_list) {
            for (l2_reg in l2_reg_list) {
              # Create and compile the model with current hyperparameters
              lstm_model_name <- paste0("lstm_model_lu_", lstm_units, "_lr_", learning_rate,
                                        "_bs_", batch_size, "_lag_", n_lag,
                                        "_do_", dropout, "_l1_", l1_reg, "_l2_", l2_reg)
              lstm_model <- keras_model_sequential(name = lstm_model_name)
              lstm_model %>%
                layer_lstm(units = lstm_units, return_sequences = TRUE, input_shape = c(1, length(embedded_colnames)),
                           dropout = dropout, recurrent_dropout = dropout,
                           kernel_regularizer = regularizer_l1_l2(l1 = l1_reg, l2 = l2_reg)) %>%
                layer_lstm(units = lstm_units, return_sequences = TRUE, dropout = dropout, recurrent_dropout = dropout,
                           kernel_regularizer = regularizer_l1_l2(l1 = l1_reg, l2 = l2_reg)) %>%
                layer_lstm(units = lstm_units, dropout = dropout, recurrent_dropout = dropout,
                           kernel_regularizer = regularizer_l1_l2(l1 = l1_reg, l2 = l2_reg)) %>%
                layer_dense(units = 1)
              lstm_model %>% compile(
                loss = custom_loss,
                optimizer = optimizer_adam(learning_rate = learning_rate),
                metrics = c("mean_absolute_error")
              )

              # Train the model with early stopping
              warning("Training", lstm_model_name, "\n")
              history <- lstm_model %>% fit(
                x = X_train,
                y = y_train,
                epochs = n_iter,
                batch_size = batch_size,
                validation_data = list(X_val, y_val),
                verbose = n_verbose,
                callbacks = list(early_stopping)
              )

              # Find the minimum val_mean_absolute_error for this combination
              min_val_loss_epoch <- which.min(history$metrics$val_loss)

              # Extract relevant information from 'history' and store in the results data frame
              results_df <- rbind(results_df, data.frame(
                lstm_units = lstm_units,
                learning_rate = learning_rate,
                batch_size = batch_size,
                n_lag = n_lag,
                dropout = dropout,
                l1_reg = l1_reg,
                l2_reg = l2_reg,
                training_loss_mae = history$metrics$mean_absolute_error[min_val_loss_epoch],
                val_loss_mae = history$metrics$val_mean_absolute_error[min_val_loss_epoch],
                early_stopping = min_val_loss_epoch
              ))

              # Store the history in the list with a unique name
              history_name <- paste0("history_lu_", lstm_units, "_lr_", learning_rate,
                                     "_bs_", batch_size, "_lag_", n_lag,
                                     "_do_", dropout, "_l1_", l1_reg, "_l2_", l2_reg)
              all_histories[[history_name]] <- history

              # Store the LSTM model in the list with a unique name
              lstm_models[[lstm_model_name]] <- lstm_model
            }
          }
        }
      }
    }
  }

  return(list(results_df = results_df, all_histories = all_histories, lstm_models = lstm_models))
}
#' Evaluate the best LSTM model on the validation set
#'
#' This function evaluates the performance of the best LSTM model on the provided validation set.
#'
#' @param best_model The best LSTM model obtained from hyperparameter tuning.
#' @param X_val The validation set input data.
#' @param y_val The validation set target data.
#'
#' @return The validation loss of the best model on the provided validation set.
#'
#' @examples
#' \donttest{
#' data <- data.frame(
#'   Date = as.Date(c("01-04-18", "02-04-18", "03-04-18", "04-04-18", "05-04-18",
#'                    "06-04-18", "07-04-18", "08-04-18", "09-04-18", "10-04-18",
#'                    "11-04-18", "12-04-18", "13-04-18", "14-04-18", "15-04-18",
#'                    "16-04-18", "17-04-18", "18-04-18", "19-04-18", "20-04-18"),
#'                  format = "%d-%m-%y"),
#'   A = c(0, 0, 4, 12, 20, 16, 16, 0, 12, 18, 12, 18, 18, 0, 0, 33, 31, 38, 76, 198)
#' )
#' check_and_format_data(data)
#' # Add a new column 'X' based on the values in the second column
#' data$X <- ifelse(data$A != 0, 1, 0)
#'
#' result_embed <- embed_columns(data = data, n_lag = 2)
#' new_data <- result_embed$data_frame
#' embedded_colnames <- result_embed$column_names
#'
#' result_split <- split_data(new_data = new_data, val_ratio = 0.1)
#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data

#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data
#' embedded_colnames <- result_embed$column_names
#' numeric_matrices <- convert_to_numeric_matrices(train_data = train_data,
#'                                                 validation_data = validation_data,
#'                                                 embedded_colnames = embedded_colnames)
#' X_train <- numeric_matrices$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- numeric_matrices$X_val
#' y_val <- numeric_matrices$y_val
#'
#' #' initialize_tensorflow()
#'
#' X_train <- numeric_matrices$X_train
#' X_val <- numeric_matrices$X_val
#' reshaped_data <- reshape_for_lstm(X_train = X_train, X_val = X_val)
#' X_train <- reshaped_data$X_train
#' X_val <- reshaped_data$X_val
#' X_train <- reshaped_data$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- reshaped_data$X_val
#' y_val <- numeric_matrices$y_val
#' tf <- reticulate::import("tensorflow")
#' tensors <- convert_to_tensors(X_train = X_train, y_train = y_train, X_val = X_val, y_val = y_val)
#' X_train <- tensors$X_train
#' y_train <- tensors$y_train
#' X_val <- tensors$X_val
#' y_val <- tensors$y_val
#' n_patience <- 50
#' early_stopping <- define_early_stopping(n_patience = n_patience)
#'
#' X_train <- tensors$X_train
#' X_val <- tensors$X_val
#'
#' y_train <- tensors$y_train
#' y_val <- tensors$y_val
#'
#' embedded_colnames <- result_embed$column_names
#'
#' # Define your custom loss function
#' custom_loss <- function(y_true, y_pred) {
#'   condition <- tf$math$equal(y_true, 0)
#'   loss <- tf$math$reduce_mean(tf$math$square(y_true - y_pred))  # Remove 'axis'
#'   loss <- tf$where(condition, tf$constant(0), loss)
#'   return(loss)
#' }
#'
#' early_stopping <- define_early_stopping(n_patience = n_patience)
#'
#' grid_search_results <- ts_lstm_x_tuning(
#'   X_train, y_train, X_val, y_val,
#'   embedded_colnames, custom_loss, early_stopping,
#'   n_lag = 2, # desired lag value
#'   lstm_units_list = c(32),
#'   learning_rate_list = c(0.001, 0.01),
#'   batch_size_list = c(32),
#'   dropout_list = c(0.2),
#'   l1_reg_list = c(0.001),
#'   l2_reg_list = c(0.001),
#'   n_iter = 10,
#'   n_verbose = 0 # or 1
#' )
#'
#' results_df <- grid_search_results$results_df
#' all_histories <- grid_search_results$all_histories
#' lstm_models <- grid_search_results$lstm_models
#'
#' # Find the row with the minimum val_loss_mae in results_df
#' min_val_loss_row <- results_df[which.min(results_df$val_loss_mae), ]
#'
#' # Extract hyperparameters from the row
#' best_lstm_units <- min_val_loss_row$lstm_units
#' best_learning_rate <- min_val_loss_row$learning_rate
#' best_batch_size <- min_val_loss_row$batch_size
#' best_n_lag <- min_val_loss_row$n_lag
#' best_dropout <- min_val_loss_row$dropout
#' best_l1_reg <- min_val_loss_row$l1_reg
#' best_l2_reg <- min_val_loss_row$l2_reg
#'
#' # Generate the lstm_model_name for the best model
#' best_model_name <- paste0("lstm_model_lu_", best_lstm_units, "_lr_", best_learning_rate,
#'                           "_bs_", best_batch_size, "_lag_", best_n_lag,
#'                           "_do_", best_dropout, "_l1_", best_l1_reg, "_l2_", best_l2_reg)
#'
#' # Generate the history_name for the best model
#' best_history_name <- paste0("history_lu_", best_lstm_units, "_lr_", best_learning_rate,
#'                             "_bs_", best_batch_size, "_lag_", best_n_lag,
#'                             "_do_", best_dropout, "_l1_", best_l1_reg, "_l2_", best_l2_reg)
#'
#' # Access the best model from lstm_models
#' best_model <- lstm_models[[best_model_name]]
#'
#' best_model_details <- data.frame(min_val_loss_row)
#'
#' colnames(best_model_details) <- colnames(results_df)
#'
#' # Access the best model from lstm_models
#' best_history <- all_histories[[best_history_name]]
#'
#' validation_loss_best <- best_model_on_validation(best_model, X_val, y_val)
#' }
#'
#' @import tensorflow
#'
#' @export
best_model_on_validation <- function(best_model, X_val, y_val) {
  validation_loss <- best_model %>% evaluate(X_val, y_val)
  warning("Validation Loss:", validation_loss, "\n")
  return(validation_loss)
}

#' Predict y values for the training and validation sets using the best LSTM model
#'
#' This function predicts y values for the training and validation sets using the provided LSTM model.
#'
#' @param best_model The best LSTM model obtained from hyperparameter tuning.
#' @param X_train The training set input data.
#' @param X_val The validation set input data.
#' @param train_data The training set data, including x values.
#' @param validation_data The validation set data, including x values.
#'
#' @return A list containing the predicted y values for the training and validation sets.
#'
#' @examples
#' \donttest{
#' data <- data.frame(
#'   Date = as.Date(c("01-04-18", "02-04-18", "03-04-18", "04-04-18", "05-04-18",
#'                    "06-04-18", "07-04-18", "08-04-18", "09-04-18", "10-04-18",
#'                    "11-04-18", "12-04-18", "13-04-18", "14-04-18", "15-04-18",
#'                    "16-04-18", "17-04-18", "18-04-18", "19-04-18", "20-04-18"),
#'                  format = "%d-%m-%y"),
#'   A = c(0, 0, 4, 12, 20, 16, 16, 0, 12, 18, 12, 18, 18, 0, 0, 33, 31, 38, 76, 198)
#' )
#' check_and_format_data(data)
#' # Add a new column 'X' based on the values in the second column
#' data$X <- ifelse(data$A != 0, 1, 0)
#'
#' result_embed <- embed_columns(data = data, n_lag = 2)
#' new_data <- result_embed$data_frame
#' embedded_colnames <- result_embed$column_names
#'
#' result_split <- split_data(new_data = new_data, val_ratio = 0.1)
#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data

#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data
#' embedded_colnames <- result_embed$column_names
#' numeric_matrices <- convert_to_numeric_matrices(train_data = train_data,
#'                                                 validation_data = validation_data,
#'                                                 embedded_colnames = embedded_colnames)
#' X_train <- numeric_matrices$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- numeric_matrices$X_val
#' y_val <- numeric_matrices$y_val
#'
#' #' initialize_tensorflow()
#'
#' X_train <- numeric_matrices$X_train
#' X_val <- numeric_matrices$X_val
#' reshaped_data <- reshape_for_lstm(X_train = X_train, X_val = X_val)
#' X_train <- reshaped_data$X_train
#' X_val <- reshaped_data$X_val
#' X_train <- reshaped_data$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- reshaped_data$X_val
#' y_val <- numeric_matrices$y_val
#' tf <- reticulate::import("tensorflow")
#' tensors <- convert_to_tensors(X_train = X_train, y_train = y_train, X_val = X_val, y_val = y_val)
#' X_train <- tensors$X_train
#' y_train <- tensors$y_train
#' X_val <- tensors$X_val
#' y_val <- tensors$y_val
#' n_patience <- 50
#' early_stopping <- define_early_stopping(n_patience = n_patience)
#'
#' X_train <- tensors$X_train
#' X_val <- tensors$X_val
#'
#' y_train <- tensors$y_train
#' y_val <- tensors$y_val
#'
#' embedded_colnames <- result_embed$column_names
#'
#' # Define your custom loss function
#' custom_loss <- function(y_true, y_pred) {
#'   condition <- tf$math$equal(y_true, 0)
#'   loss <- tf$math$reduce_mean(tf$math$square(y_true - y_pred))  # Remove 'axis'
#'   loss <- tf$where(condition, tf$constant(0), loss)
#'   return(loss)
#' }
#'
#' early_stopping <- define_early_stopping(n_patience = n_patience)
#'
#' grid_search_results <- ts_lstm_x_tuning(
#'   X_train, y_train, X_val, y_val,
#'   embedded_colnames, custom_loss, early_stopping,
#'   n_lag = 2, # desired lag value
#'   lstm_units_list = c(32),
#'   learning_rate_list = c(0.001, 0.01),
#'   batch_size_list = c(32),
#'   dropout_list = c(0.2),
#'   l1_reg_list = c(0.001),
#'   l2_reg_list = c(0.001),
#'   n_iter = 10,
#'   n_verbose = 0 # or 1
#' )
#'
#' results_df <- grid_search_results$results_df
#' all_histories <- grid_search_results$all_histories
#' lstm_models <- grid_search_results$lstm_models
#'
#' # Find the row with the minimum val_loss_mae in results_df
#' min_val_loss_row <- results_df[which.min(results_df$val_loss_mae), ]
#'
#' # Extract hyperparameters from the row
#' best_lstm_units <- min_val_loss_row$lstm_units
#' best_learning_rate <- min_val_loss_row$learning_rate
#' best_batch_size <- min_val_loss_row$batch_size
#' best_n_lag <- min_val_loss_row$n_lag
#' best_dropout <- min_val_loss_row$dropout
#' best_l1_reg <- min_val_loss_row$l1_reg
#' best_l2_reg <- min_val_loss_row$l2_reg
#'
#' # Generate the lstm_model_name for the best model
#' best_model_name <- paste0("lstm_model_lu_", best_lstm_units, "_lr_", best_learning_rate,
#'                           "_bs_", best_batch_size, "_lag_", best_n_lag,
#'                           "_do_", best_dropout, "_l1_", best_l1_reg, "_l2_", best_l2_reg)
#'
#' # Generate the history_name for the best model
#' best_history_name <- paste0("history_lu_", best_lstm_units, "_lr_", best_learning_rate,
#'                             "_bs_", best_batch_size, "_lag_", best_n_lag,
#'                             "_do_", best_dropout, "_l1_", best_l1_reg, "_l2_", best_l2_reg)
#'
#' # Access the best model from lstm_models
#' best_model <- lstm_models[[best_model_name]]
#'
#' best_model_details <- data.frame(min_val_loss_row)
#'
#' colnames(best_model_details) <- colnames(results_df)
#'
#' # Access the best model from lstm_models
#' best_history <- all_histories[[best_history_name]]
#'
#' validation_loss_best <- best_model_on_validation(best_model, X_val, y_val)
#' predicted_values <- predict_y_values(best_model, X_train, X_val, train_data, validation_data)
#' y_train_pred <- predicted_values$y_train_pred
#' y_val_pred <- predicted_values$y_val_pred
#' }
#'
#' @import tensorflow
#'
#' @export
predict_y_values <- function(best_model, X_train, X_val, train_data, validation_data) {
  y_train_pred <- best_model %>% predict(X_train)
  y_val_pred <- best_model %>% predict(X_val)
  y_train_pred <- as.numeric(y_train_pred)
  y_val_pred <- as.numeric(y_val_pred)
  y_train_pred[train_data$x == 0] <- 0
  y_val_pred[validation_data$x == 0] <- 0
  return(list(y_train_pred = y_train_pred, y_val_pred = y_val_pred))
}
#' Compare predicted and actual values for training and validation sets
#'
#' This function compares the predicted and actual values for the training and validation sets and computes metrics.
#'
#' @param train_data The training set data, including actual y values.
#' @param validation_data The validation set data, including actual y values.
#' @param y_train_pred Predicted y values for the training set.
#' @param y_val_pred Predicted y values for the validation set.
#'
#' @return A list containing data frames with the comparison of actual vs. predicted values for training and validation sets,
#' as well as metrics for the training and validation sets.
#'
#' @examples
#' \donttest{
#' data <- data.frame(
#'   Date = as.Date(c("01-04-18", "02-04-18", "03-04-18", "04-04-18", "05-04-18",
#'                    "06-04-18", "07-04-18", "08-04-18", "09-04-18", "10-04-18",
#'                    "11-04-18", "12-04-18", "13-04-18", "14-04-18", "15-04-18",
#'                    "16-04-18", "17-04-18", "18-04-18", "19-04-18", "20-04-18"),
#'                  format = "%d-%m-%y"),
#'   A = c(0, 0, 4, 12, 20, 16, 16, 0, 12, 18, 12, 18, 18, 0, 0, 33, 31, 38, 76, 198)
#' )
#' check_and_format_data(data)
#' # Add a new column 'X' based on the values in the second column
#' data$X <- ifelse(data$A != 0, 1, 0)
#'
#' result_embed <- embed_columns(data = data, n_lag = 2)
#' new_data <- result_embed$data_frame
#' embedded_colnames <- result_embed$column_names
#'
#' result_split <- split_data(new_data = new_data, val_ratio = 0.1)
#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data

#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data
#' embedded_colnames <- result_embed$column_names
#' numeric_matrices <- convert_to_numeric_matrices(train_data = train_data,
#'                                                 validation_data = validation_data,
#'                                                 embedded_colnames = embedded_colnames)
#' X_train <- numeric_matrices$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- numeric_matrices$X_val
#' y_val <- numeric_matrices$y_val
#'
#' #' initialize_tensorflow()
#'
#' X_train <- numeric_matrices$X_train
#' X_val <- numeric_matrices$X_val
#' reshaped_data <- reshape_for_lstm(X_train = X_train, X_val = X_val)
#' X_train <- reshaped_data$X_train
#' X_val <- reshaped_data$X_val
#' X_train <- reshaped_data$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- reshaped_data$X_val
#' y_val <- numeric_matrices$y_val
#' tf <- reticulate::import("tensorflow")
#' tensors <- convert_to_tensors(X_train = X_train, y_train = y_train, X_val = X_val, y_val = y_val)
#' X_train <- tensors$X_train
#' y_train <- tensors$y_train
#' X_val <- tensors$X_val
#' y_val <- tensors$y_val
#' n_patience <- 50
#' early_stopping <- define_early_stopping(n_patience = n_patience)
#'
#' X_train <- tensors$X_train
#' X_val <- tensors$X_val
#'
#' y_train <- tensors$y_train
#' y_val <- tensors$y_val
#'
#' embedded_colnames <- result_embed$column_names
#'
#' # Define your custom loss function
#' custom_loss <- function(y_true, y_pred) {
#'   condition <- tf$math$equal(y_true, 0)
#'   loss <- tf$math$reduce_mean(tf$math$square(y_true - y_pred))  # Remove 'axis'
#'   loss <- tf$where(condition, tf$constant(0), loss)
#'   return(loss)
#' }
#'
#' early_stopping <- define_early_stopping(n_patience = n_patience)
#'
#' grid_search_results <- ts_lstm_x_tuning(
#'   X_train, y_train, X_val, y_val,
#'   embedded_colnames, custom_loss, early_stopping,
#'   n_lag = 2, # desired lag value
#'   lstm_units_list = c(32),
#'   learning_rate_list = c(0.001, 0.01),
#'   batch_size_list = c(32),
#'   dropout_list = c(0.2),
#'   l1_reg_list = c(0.001),
#'   l2_reg_list = c(0.001),
#'   n_iter = 10,
#'   n_verbose = 0 # or 1
#' )
#'
#' results_df <- grid_search_results$results_df
#' all_histories <- grid_search_results$all_histories
#' lstm_models <- grid_search_results$lstm_models
#'
#' # Find the row with the minimum val_loss_mae in results_df
#' min_val_loss_row <- results_df[which.min(results_df$val_loss_mae), ]
#'
#' # Extract hyperparameters from the row
#' best_lstm_units <- min_val_loss_row$lstm_units
#' best_learning_rate <- min_val_loss_row$learning_rate
#' best_batch_size <- min_val_loss_row$batch_size
#' best_n_lag <- min_val_loss_row$n_lag
#' best_dropout <- min_val_loss_row$dropout
#' best_l1_reg <- min_val_loss_row$l1_reg
#' best_l2_reg <- min_val_loss_row$l2_reg
#'
#' # Generate the lstm_model_name for the best model
#' best_model_name <- paste0("lstm_model_lu_", best_lstm_units, "_lr_", best_learning_rate,
#'                           "_bs_", best_batch_size, "_lag_", best_n_lag,
#'                           "_do_", best_dropout, "_l1_", best_l1_reg, "_l2_", best_l2_reg)
#'
#' # Generate the history_name for the best model
#' best_history_name <- paste0("history_lu_", best_lstm_units, "_lr_", best_learning_rate,
#'                             "_bs_", best_batch_size, "_lag_", best_n_lag,
#'                             "_do_", best_dropout, "_l1_", best_l1_reg, "_l2_", best_l2_reg)
#'
#' # Access the best model from lstm_models
#' best_model <- lstm_models[[best_model_name]]
#'
#' best_model_details <- data.frame(min_val_loss_row)
#'
#' colnames(best_model_details) <- colnames(results_df)
#'
#' # Access the best model from lstm_models
#' best_history <- all_histories[[best_history_name]]
#'
#' validation_loss_best <- best_model_on_validation(best_model, X_val, y_val)
#' predicted_values <- predict_y_values(best_model, X_train, X_val, train_data, validation_data)
#' y_train_pred <- predicted_values$y_train_pred
#' y_val_pred <- predicted_values$y_val_pred
#' comparison <- compare_predicted_vs_actual(train_data, validation_data, y_train_pred, y_val_pred)
#' compare_train <- comparison$compare_train
#' compare_val <- comparison$compare_val
#' metrics_train <- comparison$metrics_train
#' metrics_val <- comparison$metrics_val
#' }
#'
#' @import AllMetrics
#' @importFrom AllMetrics all_metrics
#'
#' @export
compare_predicted_vs_actual <- function(train_data, validation_data, y_train_pred, y_val_pred) {
  compare_train <- data.frame(train_data$y, round(y_train_pred, 3))
  colnames(compare_train) <- c("actual", "predicted")
  compare_val <- data.frame(validation_data$y, round(y_val_pred, 3))
  colnames(compare_val) <- c("actual", "predicted")
  metrics_train <- all_metrics(train_data$y, y_train_pred)
  metrics_val <- all_metrics(validation_data$y, y_val_pred)
  return(list(compare_train = compare_train, compare_val = compare_val, metrics_train = metrics_train, metrics_val = metrics_val))
}
#' Perform forecasting using the best model
#'
#' This function performs forecasting using the best-trained model.
#'
#' @param best_model The best-trained LSTM model.
#' @param best_learning_rate The best learning rate used during training.
#' @param custom_loss The custom loss function used during training.
#' @param n_lag The lag value used during training.
#' @param new_data The input data for forecasting.
#' @param test The test data frame containing the input data for forecasting.
#' @param forecast_steps The number of steps to forecast.
#'
#' @return A list containing the forecasted values, actual vs. forecasted data frame, and metrics for forecasting.
#'
#' @examples
#' \donttest{
#' data <- data.frame(
#'   Date = as.Date(c("01-04-18", "02-04-18", "03-04-18", "04-04-18", "05-04-18",
#'                    "06-04-18", "07-04-18", "08-04-18", "09-04-18", "10-04-18",
#'                    "11-04-18", "12-04-18", "13-04-18", "14-04-18", "15-04-18",
#'                    "16-04-18", "17-04-18", "18-04-18", "19-04-18", "20-04-18"),
#'                  format = "%d-%m-%y"),
#'   A = c(0, 0, 4, 12, 20, 16, 16, 0, 12, 18, 12, 18, 18, 0, 0, 33, 31, 38, 76, 198)
#' )
#' check_and_format_data(data)
#' # Add a new column 'X' based on the values in the second column
#' data$X <- ifelse(data$A != 0, 1, 0)
#'
#' result_embed <- embed_columns(data = data, n_lag = 2)
#' new_data <- result_embed$data_frame
#' embedded_colnames <- result_embed$column_names
#'
#' result_split <- split_data(new_data = new_data, val_ratio = 0.1)
#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data

#' train_data <- result_split$train_data
#' validation_data <- result_split$validation_data
#' embedded_colnames <- result_embed$column_names
#' numeric_matrices <- convert_to_numeric_matrices(train_data = train_data,
#'                                                 validation_data = validation_data,
#'                                                 embedded_colnames = embedded_colnames)
#' X_train <- numeric_matrices$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- numeric_matrices$X_val
#' y_val <- numeric_matrices$y_val
#'
#' #' initialize_tensorflow()
#'
#' X_train <- numeric_matrices$X_train
#' X_val <- numeric_matrices$X_val
#' reshaped_data <- reshape_for_lstm(X_train = X_train, X_val = X_val)
#' X_train <- reshaped_data$X_train
#' X_val <- reshaped_data$X_val
#' X_train <- reshaped_data$X_train
#' y_train <- numeric_matrices$y_train
#' X_val <- reshaped_data$X_val
#' y_val <- numeric_matrices$y_val
#' tf <- reticulate::import("tensorflow")
#' tensors <- convert_to_tensors(X_train = X_train, y_train = y_train, X_val = X_val, y_val = y_val)
#' X_train <- tensors$X_train
#' y_train <- tensors$y_train
#' X_val <- tensors$X_val
#' y_val <- tensors$y_val
#' n_patience <- 50
#' early_stopping <- define_early_stopping(n_patience = n_patience)
#'
#' X_train <- tensors$X_train
#' X_val <- tensors$X_val
#'
#' y_train <- tensors$y_train
#' y_val <- tensors$y_val
#'
#' embedded_colnames <- result_embed$column_names
#'
#' # Define your custom loss function
#' custom_loss <- function(y_true, y_pred) {
#'   condition <- tf$math$equal(y_true, 0)
#'   loss <- tf$math$reduce_mean(tf$math$square(y_true - y_pred))  # Remove 'axis'
#'   loss <- tf$where(condition, tf$constant(0), loss)
#'   return(loss)
#' }
#'
#' early_stopping <- define_early_stopping(n_patience = n_patience)
#'
#' grid_search_results <- ts_lstm_x_tuning(
#'   X_train, y_train, X_val, y_val,
#'   embedded_colnames, custom_loss, early_stopping,
#'   n_lag = 2, # desired lag value
#'   lstm_units_list = c(32),
#'   learning_rate_list = c(0.001, 0.01),
#'   batch_size_list = c(32),
#'   dropout_list = c(0.2),
#'   l1_reg_list = c(0.001),
#'   l2_reg_list = c(0.001),
#'   n_iter = 10,
#'   n_verbose = 0 # or 1
#' )
#'
#' results_df <- grid_search_results$results_df
#' all_histories <- grid_search_results$all_histories
#' lstm_models <- grid_search_results$lstm_models
#'
#' # Find the row with the minimum val_loss_mae in results_df
#' min_val_loss_row <- results_df[which.min(results_df$val_loss_mae), ]
#'
#' # Extract hyperparameters from the row
#' best_lstm_units <- min_val_loss_row$lstm_units
#' best_learning_rate <- min_val_loss_row$learning_rate
#' best_batch_size <- min_val_loss_row$batch_size
#' best_n_lag <- min_val_loss_row$n_lag
#' best_dropout <- min_val_loss_row$dropout
#' best_l1_reg <- min_val_loss_row$l1_reg
#' best_l2_reg <- min_val_loss_row$l2_reg
#'
#' # Generate the lstm_model_name for the best model
#' best_model_name <- paste0("lstm_model_lu_", best_lstm_units, "_lr_", best_learning_rate,
#'                           "_bs_", best_batch_size, "_lag_", best_n_lag,
#'                           "_do_", best_dropout, "_l1_", best_l1_reg, "_l2_", best_l2_reg)
#'
#' # Generate the history_name for the best model
#' best_history_name <- paste0("history_lu_", best_lstm_units, "_lr_", best_learning_rate,
#'                             "_bs_", best_batch_size, "_lag_", best_n_lag,
#'                             "_do_", best_dropout, "_l1_", best_l1_reg, "_l2_", best_l2_reg)
#'
#' # Access the best model from lstm_models
#' best_model <- lstm_models[[best_model_name]]
#'
#' best_model_details <- data.frame(min_val_loss_row)
#'
#' colnames(best_model_details) <- colnames(results_df)
#'
#' # Access the best model from lstm_models
#' best_history <- all_histories[[best_history_name]]
#'
#' validation_loss_best <- best_model_on_validation(best_model, X_val, y_val)
#' predicted_values <- predict_y_values(best_model, X_train, X_val, train_data, validation_data)
#' y_train_pred <- predicted_values$y_train_pred
#' y_val_pred <- predicted_values$y_val_pred
#' comparison <- compare_predicted_vs_actual(train_data, validation_data, y_train_pred, y_val_pred)
#' compare_train <- comparison$compare_train
#' compare_val <- comparison$compare_val
#' metrics_train <- comparison$metrics_train
#' metrics_val <- comparison$metrics_val
#'
#' test <- data.frame(
#'   Date = as.Date(c("01-04-23", "02-04-23", "03-04-23", "04-04-23", "05-04-23",
#'                    "06-04-23", "07-04-23", "08-04-23", "09-04-23", "10-04-23",
#'                    "11-04-23", "12-04-23", "13-04-23", "14-04-23", "15-04-23",
#'                    "16-04-23", "17-04-23", "18-04-23", "19-04-23", "20-04-23"),
#'                  format = "%d-%m-%y"),
#'   A = c(0, 0, 15, 4, -31, 24, 14, 0, 0, 33, 38, 33, 29, 29, 25, 0, 44, 67, 162, 278)
#' )
#'
#' test$X <- ifelse(test$A != 0, 1, 0)
#'
#' n_forecast <- nrow(test)
#'
#' # Perform one-step-ahead forecasting
#' forecast_steps <- n_forecast
#' current_row <- nrow(new_data)
#' forecast_results <- forecast_best_model(best_model, best_learning_rate,
#'                                         custom_loss, n_lag = 2,
#'                                         new_data, test,
#'                                         forecast_steps)
#'
#' # Access the results
#' forecast_values <- forecast_results$forecast_values
#' actual_vs_forecast <- forecast_results$actual_vs_forecast
#' metrics_forecast <- forecast_results$metrics_forecast
#' }
#'
#' @import AllMetrics
#' @importFrom AllMetrics all_metrics
#' @importFrom keras clone_model
#'
#' @export
forecast_best_model <- function(
    best_model,
    best_learning_rate,
    custom_loss,
    n_lag = 2,
    new_data,
    test,
    forecast_steps
) {
  forecast_model <- clone_model(best_model)
  forecast_model$set_weights(best_model$get_weights())

  # Compile the forecasting model
  forecast_model %>% compile(
    loss = custom_loss,
    optimizer = optimizer_adam(learning_rate = best_learning_rate),
    metrics = c("mean_absolute_error")
  )

  current_row <- nrow(new_data)

  for (i in 1:forecast_steps) {
    forecast_input <- as.matrix(new_data[current_row, c("y", paste0("y", 1:(n_lag - 1)), "x")])
    forecast_input <- array_reshape(forecast_input, c(1, 1, length(forecast_input)))

    next_value <- forecast_model %>% predict(forecast_input)
    next_value[test$X[i] == 0] <- 0

    new_data[current_row + 1, "y"] <- next_value
    new_data[current_row + 1, "y1"] <- new_data[current_row, "y"]
    for (j in 1:(n_lag - 1)) {
      new_data[current_row + 1, paste0("y", j + 1)] <- new_data[current_row, paste0("y", j)]
    }
    new_data[current_row + 1, "x"] <- test$X[i]

    test$forecasted_A[i] <- round(next_value, 3)

    current_row <- current_row + 1
    forecast_model$set_weights(forecast_model %>% get_weights())
  }

  forecast_values <- test$forecasted_A[1:forecast_steps]
  actual_vs_forecast <- test[1:forecast_steps, ]

  # forecast_loss <- forecast_model %>% evaluate(tf$constant(X_val), tf$constant(y_val))
  # warning("Forecasting Loss on Validation Set:", forecast_loss, "\n")

  metrics_forecast <- all_metrics(test$A[1:forecast_steps], test$forecasted_A[1:forecast_steps])

  return(list(forecast_values = forecast_values, actual_vs_forecast = actual_vs_forecast, metrics_forecast = metrics_forecast))
}

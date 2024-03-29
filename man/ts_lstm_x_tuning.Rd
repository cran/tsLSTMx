% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compiled_code_tslstmx.R
\name{ts_lstm_x_tuning}
\alias{ts_lstm_x_tuning}
\title{Time Series LSTM Hyperparameter Tuning}
\usage{
ts_lstm_x_tuning(
  X_train,
  y_train,
  X_val,
  y_val,
  embedded_colnames,
  custom_loss,
  early_stopping,
  n_lag = 2,
  lstm_units_list = c(32),
  learning_rate_list = c(0.001, 0.01),
  batch_size_list = c(32),
  dropout_list = c(0.2),
  l1_reg_list = c(0.001),
  l2_reg_list = c(0.001),
  n_iter = 10,
  n_verbose = 0
)
}
\arguments{
\item{X_train}{Numeric matrix, the training input data.}

\item{y_train}{Numeric vector, the training target data.}

\item{X_val}{Numeric matrix, the validation input data.}

\item{y_val}{Numeric vector, the validation target data.}

\item{embedded_colnames}{Character vector, column names of the embedded features.}

\item{custom_loss}{Function, custom loss function for the LSTM model.}

\item{early_stopping}{keras early stopping callback.}

\item{n_lag}{Integer, desired lag value.}

\item{lstm_units_list}{Numeric vector, list of LSTM units to search over.}

\item{learning_rate_list}{Numeric vector, list of learning rates to search over.}

\item{batch_size_list}{Numeric vector, list of batch sizes to search over.}

\item{dropout_list}{Numeric vector, list of dropout rates to search over.}

\item{l1_reg_list}{Numeric vector, list of L1 regularization values to search over.}

\item{l2_reg_list}{Numeric vector, list of L2 regularization values to search over.}

\item{n_iter}{Integer, number of epochs for each model training.}

\item{n_verbose}{Integer, level of verbosity during training (0 or 1).}
}
\value{
A list containing the results data frame, all histories, and LSTM models.
}
\description{
This function performs hyperparameter tuning for a Time Series LSTM model using a grid search approach.
}
\examples{
\donttest{
data <- data.frame(
  Date = as.Date(c("01-04-18", "02-04-18", "03-04-18", "04-04-18", "05-04-18",
                   "06-04-18", "07-04-18", "08-04-18", "09-04-18", "10-04-18",
                   "11-04-18", "12-04-18", "13-04-18", "14-04-18", "15-04-18",
                   "16-04-18", "17-04-18", "18-04-18", "19-04-18", "20-04-18"),
                 format = "\%d-\%m-\%y"),
  A = c(0, 0, 4, 12, 20, 16, 16, 0, 12, 18, 12, 18, 18, 0, 0, 33, 31, 38, 76, 198)
)
check_and_format_data(data)
# Add a new column 'X' based on the values in the second column
data$X <- ifelse(data$A != 0, 1, 0)

result_embed <- embed_columns(data = data, n_lag = 2)
new_data <- result_embed$data_frame
embedded_colnames <- result_embed$column_names

result_split <- split_data(new_data = new_data, val_ratio = 0.1)
train_data <- result_split$train_data
validation_data <- result_split$validation_data
train_data <- result_split$train_data
validation_data <- result_split$validation_data
embedded_colnames <- result_embed$column_names
numeric_matrices <- convert_to_numeric_matrices(train_data = train_data,
                                                validation_data = validation_data,
                                                embedded_colnames = embedded_colnames)
X_train <- numeric_matrices$X_train
y_train <- numeric_matrices$y_train
X_val <- numeric_matrices$X_val
y_val <- numeric_matrices$y_val

#' initialize_tensorflow()

X_train <- numeric_matrices$X_train
X_val <- numeric_matrices$X_val
reshaped_data <- reshape_for_lstm(X_train = X_train, X_val = X_val)
X_train <- reshaped_data$X_train
X_val <- reshaped_data$X_val
X_train <- reshaped_data$X_train
y_train <- numeric_matrices$y_train
X_val <- reshaped_data$X_val
y_val <- numeric_matrices$y_val
tf <- reticulate::import("tensorflow")
tensors <- convert_to_tensors(X_train = X_train, y_train = y_train, X_val = X_val, y_val = y_val)
X_train <- tensors$X_train
y_train <- tensors$y_train
X_val <- tensors$X_val
y_val <- tensors$y_val
n_patience <- 50
early_stopping <- define_early_stopping(n_patience = n_patience)

X_train <- tensors$X_train
X_val <- tensors$X_val

y_train <- tensors$y_train
y_val <- tensors$y_val

embedded_colnames <- result_embed$column_names

# Define your custom loss function
custom_loss <- function(y_true, y_pred) {
  condition <- tf$math$equal(y_true, 0)
  loss <- tf$math$reduce_mean(tf$math$square(y_true - y_pred))  # Remove 'axis'
  loss <- tf$where(condition, tf$constant(0), loss)
  return(loss)
}

early_stopping <- define_early_stopping(n_patience = n_patience)

grid_search_results <- ts_lstm_x_tuning(
  X_train, y_train, X_val, y_val,
  embedded_colnames, custom_loss, early_stopping,
  n_lag = 2, # desired lag value
  lstm_units_list = c(32),
  learning_rate_list = c(0.001, 0.01),
  batch_size_list = c(32),
  dropout_list = c(0.2),
  l1_reg_list = c(0.001),
  l2_reg_list = c(0.001),
  n_iter = 10,
  n_verbose = 0 # or 1
)

results_df <- grid_search_results$results_df
all_histories <- grid_search_results$all_histories
lstm_models <- grid_search_results$lstm_models

# Find the row with the minimum val_loss_mae in results_df
min_val_loss_row <- results_df[which.min(results_df$val_loss_mae), ]

# Extract hyperparameters from the row
best_lstm_units <- min_val_loss_row$lstm_units
best_learning_rate <- min_val_loss_row$learning_rate
best_batch_size <- min_val_loss_row$batch_size
best_n_lag <- min_val_loss_row$n_lag
best_dropout <- min_val_loss_row$dropout
best_l1_reg <- min_val_loss_row$l1_reg
best_l2_reg <- min_val_loss_row$l2_reg

# Generate the lstm_model_name for the best model
best_model_name <- paste0("lstm_model_lu_", best_lstm_units, "_lr_", best_learning_rate,
                          "_bs_", best_batch_size, "_lag_", best_n_lag,
                          "_do_", best_dropout, "_l1_", best_l1_reg, "_l2_", best_l2_reg)

# Generate the history_name for the best model
best_history_name <- paste0("history_lu_", best_lstm_units, "_lr_", best_learning_rate,
                            "_bs_", best_batch_size, "_lag_", best_n_lag,
                            "_do_", best_dropout, "_l1_", best_l1_reg, "_l2_", best_l2_reg)

# Access the best model from lstm_models
best_model <- lstm_models[[best_model_name]]

best_model_details <- data.frame(min_val_loss_row)

colnames(best_model_details) <- colnames(results_df)

# Access the best model from lstm_models
best_history <- all_histories[[best_history_name]]
}

}
\references{
Garai, S., & Paul, R. K. (2023). Development of MCS based-ensemble models using CEEMDAN decomposition and machine intelligence. Intelligent Systems with Applications, 18, 200202.
}

# ------------------------------------------------------------
# Title: 07b-model_training_xgb.R
# Author: Ashley Sanders
# Date: 2025-04-22
# Description: Trains and tunes an XGBoost regression model to forecast deal amounts using lead and engagement features.
# Input: sales_model.csv
# Output: Trained model object (xgb_tuned), performance metrics
# Dependencies: tidyverse, here, xgboost, caret, pROC
# ------------------------------------------------------------

library(here)
library(tidyverse)
library(caret)
library(xgboost)
library(pROC)

# Load prepared training data
sales_model <- read_csv(here("data/processed", "sales_model.csv")) %>% clean_names()

# Create model matrix and response vector
x_matrix <- model.matrix(deal_amount ~ . -deal_id -close_date_parsed -close_month, data = sales_model)
y_vector <- sales_model$deal_amount
weights <- sales_model$lead_score_tier_num

# Train/test split
set.seed(42)
train_idx <- createDataPartition(y_vector, p = 0.8, list = FALSE)
x_train <- x_matrix[train_idx, ]
x_test <- x_matrix[-train_idx, ]
y_train <- y_vector[train_idx]
y_test <- y_vector[-train_idx]
w_train <- weights[train_idx]
w_test <- weights[-train_idx]

# Training control
tune_ctrl <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE
)

# Tuning grid
xgb_grid <- expand.grid(
  nrounds = c(100, 200),
  max_depth = c(3, 6),
  eta = c(0.1, 0.3),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# Train model
set.seed(42)
xgb_tuned <- train(
  x = x_train,
  y = y_train,
  method = "xgbTree",
  trControl = tune_ctrl,
  tuneGrid = xgb_grid,
  weights = w_train
)

# Evaluate
preds_test <- predict(xgb_tuned, newdata = x_test)
rmse <- sqrt(mean((preds_test - y_test)^2))
r2 <- cor(preds_test, y_test)^2

cat("RMSE:", rmse, "\n")
cat("R-squared:", r2, "\n")

# Save model for reuse
saveRDS(xgb_tuned, here("data/processed", "xgb_model.rds"))

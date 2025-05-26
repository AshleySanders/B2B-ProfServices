# ------------------------------------------------------------
# Title: 04c-lead_model_xgb.R
# Author: Ashley Sanders
# Date: 2025-04-30
# Description: Builds, tunes, and evaluates an XGBoost model to predict lead conversion and score contacts.
# Input: Processed engagement data and deal outcome data
# Output: Tuned model, ROC curve, scored leads with confidence bands and tiers
# Dependencies: tidyverse, here, caret, pROC, janitor, forcats, xgboost, Matrix
# ------------------------------------------------------------

library(here)
library(tidyverse)
library(janitor)
library(forcats)
library(caret)
library(pROC)
library(xgboost)
library(Matrix)

# Load data
engagement <- read_csv(here("data/processed", "engagement_all.csv")) %>%
  clean_names()

closed_won <- read_csv(here("data/raw", "closed_won_by_contact.csv")) %>%
  clean_names()

# Join and prep
lead_data <- engagement %>%
  left_join(closed_won, by = "contact_id") %>%
  mutate(closed_won_count = replace_na(closed_won_count, 0)) %>%
  mutate(
    closed_won_count = case_when(
      closed_won_count >= 1 ~ "yes",
      TRUE ~ "no"
    ),
    closed_won_count = factor(closed_won_count, levels = c("no", "yes"))
  )

model_data <- lead_data %>%
  select(-c(contact_id, email)) %>%
  mutate(
    role = fct_na_value_to_level(as.factor(role), level = "unknown"),
    lifecycle_stage = fct_na_value_to_level(as.factor(lifecycle_stage), level = "unknown")
  )

# Train/test split
set.seed(123)
train_index <- createDataPartition(model_data$closed_won_count, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Create model matrix
model_formula <- closed_won_count ~ .
x_train <- model.matrix(model_formula, data = train_data)[, -1]
x_test <- model.matrix(model_formula, data = test_data)[, -1]
y_train <- train_data$closed_won_count
y_test <- test_data$closed_won_count

# Train control
ctrl <- trainControl(
  method = "cv",
  number = 5,
  sampling = "up",
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Tune XGBoost
xgb_grid <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 1),
  colsample_bytree = c(0.6, 0.8, 1.0),
  min_child_weight = c(1, 5),
  subsample = c(0.7, 1.0)
)

xgb_model <- train(
  x = x_train,
  y = y_train,
  method = "xgbTree",
  metric = "ROC",
  trControl = ctrl,
  tuneGrid = xgb_grid
)

# Evaluate
xgb_preds <- predict(xgb_model, newdata = x_test, type = "prob")
xgb_roc <- roc(response = y_test, predictor = xgb_preds$yes)
auc(xgb_roc)
plot(xgb_roc, main = "ROC - XGBoost Tuned")

# Score and tier leads
test_scored_xgb <- test_data %>%
  mutate(predicted_prob = xgb_preds$yes) %>%
  mutate(
    lead_score_xgb = round(predicted_prob * 100),
    lead_score_lower = pmax(0, lead_score_xgb - 10),
    lead_score_upper = pmin(100, lead_score_xgb + 10),
    lead_score_tier = case_when(
      predicted_prob >= 0.7 ~ "High",
      predicted_prob >= 0.4 ~ "Medium",
      TRUE ~ "Low"
    )
  )

# Export results
write_csv(test_scored_xgb, here("data/processed", "lead_scores_xgb.csv"))

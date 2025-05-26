# ------------------------------------------------------------
# Title: 04b-lead_model_logit_rf.R
# Author: Ashley Sanders
# Date: 2025-04-30
# Description: Builds logistic regression and random forest models to predict lead conversion based on engagement data.
# Input: Processed engagement data and deal outcome data
# Output: Trained models, performance metrics, and scored leads with tier assignments
# Dependencies: tidyverse, here, caret, pROC, janitor, forcats
# ------------------------------------------------------------

library(here)
library(tidyverse)
library(janitor)
library(forcats)
library(caret)
library(pROC)

# Load data
engagement <- read_csv(here("data/processed", "engagement_all.csv")) %>%
  clean_names()

# Load closed-won counts per contact
closed_won <- read_csv(here("data/raw", "closed_won_by_contact.csv")) %>%
  clean_names()

# Join engagement data with conversion outcomes
lead_data <- engagement %>%
  left_join(closed_won, by = "contact_id") %>%
  mutate(closed_won_count = replace_na(closed_won_count, 0))

# Define outcome variable
lead_data <- lead_data %>%
  mutate(
    closed_won_count = case_when(
      closed_won_count >= 1 ~ "yes",
      TRUE ~ "no"
    ),
    closed_won_count = factor(closed_won_count, levels = c("no", "yes"))
  )

# Prepare data for modeling
model_data <- lead_data %>%
  select(-c(contact_id, email)) %>%
  mutate(
    role = fct_na_value_to_level(as.factor(role), level = "unknown"),
    lifecycle_stage = fct_na_value_to_level(as.factor(lifecycle_stage), level = "unknown")
  )

# Set up train/test split
set.seed(123)
train_index <- createDataPartition(model_data$closed_won_count, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Train control setup
ctrl <- trainControl(
  method = "cv",
  number = 5,
  sampling = "up",
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

# Train logistic regression model
logit_model <- train(
  closed_won_count ~ .,
  data = train_data,
  method = "glm",
  family = "binomial",
  metric = "ROC",
  trControl = ctrl
)

# Evaluate on test data
logit_preds <- predict(logit_model, newdata = test_data, type = "prob")
roc_logit <- roc(response = test_data$closed_won_count, predictor = logit_preds$yes)
auc_logit <- auc(roc_logit)
print(auc_logit)
plot(roc_logit, main = "ROC - Logistic Regression")

# Train random forest model
rf_model <- train(
  closed_won_count ~ .,
  data = train_data,
  method = "ranger",
  metric = "ROC",
  trControl = ctrl,
  importance = "impurity"
)

# Evaluate random forest
rf_preds <- predict(rf_model, newdata = test_data, type = "prob")
roc_rf <- roc(response = test_data$closed_won_count, predictor = rf_preds$yes)
auc_rf <- auc(roc_rf)
print(auc_rf)
plot(roc_rf, main = "ROC - Random Forest")

# Score leads and assign tiers
test_scored <- test_data %>%
  mutate(predicted_prob = rf_preds$yes) %>%
  mutate(
    lead_score = round(predicted_prob * 100),
    lead_score_tier = case_when(
      predicted_prob >= 0.7 ~ "High",
      predicted_prob >= 0.4 ~ "Medium",
      TRUE ~ "Low"
    )
  )

# Export scored leads
write_csv(test_scored, here("data/processed", "lead_scores_rf.csv"))

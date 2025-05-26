# ------------------------------------------------------------
# Title: 04d-apply_scoring_all_contacts.R
# Author: Ashley Sanders
# Date: 2025-04-30
# Description: Applies tuned XGBoost model to all contacts and assigns lead scores, tiers, and confidence bands.
# Input: engagement_all.csv, trained xgb_model
# Output: all_contacts_scored.csv with full lead scoring details
# Dependencies: tidyverse, here, janitor, forcats, Matrix, xgboost
# ------------------------------------------------------------

library(here)
library(tidyverse)
library(janitor)
library(forcats)
library(Matrix)
library(xgboost)

# Load full engagement dataset
engagement_all <- read_csv(here("data/processed", "engagement_all.csv")) %>%
  clean_names()

# Prepare data (match training types and levels)
scoring_data <- engagement_all %>%
  mutate(
    role = fct_na_value_to_level(as.factor(role), level = "unknown"),
    lifecycle_stage = fct_na_value_to_level(as.factor(lifecycle_stage), level = "unknown"),
    marketing_emails_opened = as.numeric(marketing_emails_opened),
    marketing_emails_clicked = as.numeric(marketing_emails_clicked),
    marketing_emails_replied = as.numeric(marketing_emails_replied),
    currently_in_sequence = as.numeric(currently_in_sequence)
  )

# Match factor levels to training set
for (col in names(train_data)) {
  if (is.factor(train_data[[col]])) {
    scoring_data[[col]] <- factor(scoring_data[[col]], levels = levels(train_data[[col]]))
  }
}

# Create model matrix using same formula
new_contacts <- model.matrix(model_formula, data = scoring_data)[, -1]

# Predict probabilities
xgb_preds_all <- predict(xgb_model, newdata = new_contacts, type = "prob")

# Score and assign tiers
contacts_scored <- engagement_all %>%
  mutate(predicted_prob_xgb = xgb_preds_all$yes) %>%
  mutate(
    lead_score_xgb = round(predicted_prob_xgb * 100, 0),
    lead_score_lower_xgb = pmax(0, lead_score_xgb - 10),
    lead_score_upper_xgb = pmin(100, lead_score_xgb + 10),
    quantile_rank = ntile(predicted_prob_xgb, 100),
    lead_score_tier_xgb = case_when(
      quantile_rank > 70 ~ "High",
      quantile_rank > 40 ~ "Medium",
      TRUE ~ "Low"
    )
  )

# Export
write_csv(contacts_scored, here("data/processed", "all_contacts_scored.csv"))

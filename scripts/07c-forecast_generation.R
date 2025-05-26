# ------------------------------------------------------------
# Title: 07c-forecast_generation.R
# Author: Ashley Sanders
# Date: 2025-04-22
# Description: Applies trained XGBoost model to score future (open) deals and generate monthly revenue forecasts.
# Input: open_deals.csv, engagement_all.csv, companies_unique.csv, xgb_model.rds
# Output: forecast_data_summary.csv
# Dependencies: tidyverse, here, lubridate, janitor, forcats, xgboost
# ------------------------------------------------------------

library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(forcats)
library(xgboost)

# Load model and inputs
xgb_model <- readRDS(here("data/processed", "xgb_model.rds"))
open_deals <- read_csv(here("data/raw", "open_deals.csv")) %>% clean_names()
engagement <- read_csv(here("data/processed", "engagement_all.csv")) %>% clean_names()
companies <- read_csv(here("data/raw", "companies_unique.csv")) %>% clean_names()

# Join engagement and company info
forecast_data <- open_deals %>%
  left_join(engagement, by = "contact_id") %>%
  left_join(companies %>% select(primary_company_id, primary_industry), by = "primary_company_id")

# Feature engineering
forecast_data <- forecast_data %>%
  mutate(
    close_date_parsed = ymd(close_date),
    close_month = floor_date(close_date_parsed, unit = "month"),
    deal_age = as.numeric(difftime(close_date_parsed, create_date_parsed, units = "days")),
    primary_industry = fct_na_value_to_level(as.factor(primary_industry), level = "unknown"),
    lifecycle_stage = fct_na_value_to_level(as.factor(lifecycle_stage), level = "unknown"),
    role = fct_na_value_to_level(as.factor(role), level = "unknown"),
    job_domain = fct_na_value_to_level(as.factor(job_domain), level = "unknown"),
    engagement_score =
      0.2 * number_of_pageviews +
      0.2 * number_of_sessions +
      0.4 * number_of_form_submissions +
      0.5 * marketing_emails_opened +
      1.0 * marketing_emails_clicked +
      1.5 * marketing_emails_replied +
      2.0 * if_else(!is.na(recent_sales_email_replied_date), 1, 0) +
      1.0 * currently_in_sequence
  )

# Prepare forecast matrix
model_formula <- deal_amount ~ . -deal_id -close_date_parsed -close_month
forecast_matrix <- model.matrix(model_formula, data = forecast_data)

# Align forecast matrix columns to match training matrix
train_matrix <- model.matrix(model_formula, data = read_csv(here("data/processed", "sales_model.csv")) %>% clean_names())
train_cols <- colnames(train_matrix)

# Match column order and fill missing with 0
forecast_matrix_aligned <- matrix(0, nrow = nrow(forecast_matrix), ncol = length(train_cols))
colnames(forecast_matrix_aligned) <- train_cols
common_cols <- intersect(colnames(forecast_matrix), train_cols)
forecast_matrix_aligned[, common_cols] <- forecast_matrix[, common_cols]

# Predict
forecast_preds <- predict(xgb_model, newdata = forecast_matrix_aligned)

# Combine predictions and aggregate
forecast_data <- forecast_data %>%
  mutate(predicted_deal_amount = forecast_preds)

forecast_data_summary <- forecast_data %>%
  mutate(close_month = floor_date(close_date_parsed, unit = "month")) %>%
  group_by(close_month) %>%
  summarise(
    n_deals = n(),
    total_predicted_sales = sum(predicted_deal_amount, na.rm = TRUE),
    avg_predicted_deal = mean(predicted_deal_amount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(close_month)

# Save forecast output
write_csv(forecast_data_summary, here("data/processed", "forecast_data_summary.csv"))

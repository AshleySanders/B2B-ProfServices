# ------------------------------------------------------------
# Title: 07a-data_preparation.R
# Author: Ashley Sanders
# Date: 2025-04-22
# Description: Cleans and prepares historical sales and lead engagement data for XGBoost forecasting.
# Input: all_deals_R.csv, engagement_all.csv, companies_unique.csv
# Output: sales_model.csv
# Dependencies: tidyverse, here, janitor, lubridate, forcats
# ------------------------------------------------------------

library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(forcats)

# Load raw data
deals <- read_csv(here("data/raw", "all_deals_R.csv")) %>% clean_names()
engagement <- read_csv(here("data/processed", "engagement_all.csv")) %>% clean_names()
companies <- read_csv(here("data/raw", "companies_unique.csv")) %>% clean_names()

# Remove duplicates
deals <- deals %>% distinct(deal_id, .keep_all = TRUE)
engagement <- engagement %>% distinct(contact_id, .keep_all = TRUE)

# Join engagement and company metadata
deals <- left_join(deals, engagement, by = "contact_id")
deals <- left_join(deals, companies %>% select(primary_company_id, primary_industry), by = "primary_company_id")

# Feature engineering
deals <- deals %>%
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

# Filter: training data only for closed deals with valid amount
deals_filtered <- deals %>%
  filter(
    !is.na(deal_amount),
    deal_amount > 0,
    !is.na(close_date_parsed),
    !is.na(engagement_score)
  )

# Assign lead score tiers if not already present
deals_filtered <- deals_filtered %>%
  mutate(
    lead_score_tier_xgb = case_when(
      predicted_prob_xgb >= 0.7 ~ "High",
      predicted_prob_xgb >= 0.4 ~ "Medium",
      TRUE ~ "Low"
    ),
    lead_score_tier_num = case_when(
      lead_score_tier_xgb == "High" ~ 3,
      lead_score_tier_xgb == "Medium" ~ 2,
      TRUE ~ 1
    )
  )

# Final variable selection
sales_model <- deals_filtered %>%
  select(
    deal_id, close_date_parsed, close_month, deal_amount,
    engagement_score, role, lifecycle_stage, job_domain,
    primary_industry, lead_score_tier_xgb, lead_score_tier_num,
    number_of_times_contacted, number_of_sales_activities,
    deal_age, predicted_prob_xgb
  )

# Save cleaned dataset
write_csv(sales_model, here("data/processed", "sales_model.csv"))

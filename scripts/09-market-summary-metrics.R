# ------------------------------------------------------------
# Title: 09-market_summary_metrics.R
# Author: Ashley Sanders
# Date: 2025-04-25
# Description: Generates summary performance metrics by lead tier, industry, geography, and channel for strategic segmentation.
# Input: sales_model.csv
# Output: Multiple summary tables by market segment
# Dependencies: tidyverse, here, janitor, lubridate
# ------------------------------------------------------------

library(here)
library(tidyverse)
library(janitor)
library(lubridate)

# Load cleaned dataset
sales_model <- read_csv(here("data/processed", "sales_model.csv")) %>% clean_names()

# Add geographic and temporal features
sales_model <- sales_model %>%
  mutate(
    year = year(close_date_parsed),
    month = month(close_date_parsed, label = TRUE, abbr = TRUE)
  )

# Summary by lead score tier
summary_by_tier <- sales_model %>%
  group_by(lead_score_tier_xgb) %>%
  summarise(
    deals = n(),
    avg_deal = mean(deal_amount, na.rm = TRUE),
    total_sales = sum(deal_amount, na.rm = TRUE),
    avg_engagement = mean(engagement_score, na.rm = TRUE),
    .groups = "drop"
  )

# Summary by industry
summary_by_industry <- sales_model %>%
  group_by(primary_industry) %>%
  summarise(
    deals = n(),
    avg_deal = mean(deal_amount, na.rm = TRUE),
    total_sales = sum(deal_amount, na.rm = TRUE),
    avg_engagement = mean(engagement_score, na.rm = TRUE),
    .groups = "drop"
  )

# Summary by role and domain
summary_by_role <- sales_model %>%
  group_by(role) %>%
  summarise(deals = n(), avg_deal = mean(deal_amount), .groups = "drop")

summary_by_domain <- sales_model %>%
  group_by(job_domain) %>%
  summarise(deals = n(), avg_deal = mean(deal_amount), .groups = "drop")

# Summary by year + tier
summary_by_year_tier <- sales_model %>%
  group_by(year, lead_score_tier_xgb) %>%
  summarise(
    deals = n(),
    total_sales = sum(deal_amount),
    avg_deal = mean(deal_amount),
    .groups = "drop"
  )

# Export summaries
write_csv(summary_by_tier, here("outputs/tables", "summary_by_tier.csv"))
write_csv(summary_by_industry, here("outputs/tables", "summary_by_industry.csv"))
write_csv(summary_by_role, here("outputs/tables", "summary_by_role.csv"))
write_csv(summary_by_domain, here("outputs/tables", "summary_by_domain.csv"))
write_csv(summary_by_year_tier, here("outputs/tables", "summary_by_year_tier.csv"))

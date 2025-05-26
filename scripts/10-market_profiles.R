# ------------------------------------------------------------
# Title: 10-market_profiles.R
# Author: Ashley Sanders
# Date: 2025-04-25
# Description: Creates detailed market profiles by aggregating sales metrics by tier and industry. Highlights most valuable segments.
# Input: sales_model.csv
# Output: Segmented profile tables for GTM strategy
# Dependencies: tidyverse, here, janitor, lubridate
# ------------------------------------------------------------

library(here)
library(tidyverse)
library(janitor)
library(lubridate)

# Load data
sales_model <- read_csv(here("data/processed", "sales_model.csv")) %>% clean_names()

# Create profile summary by tier + industry
profile_summary <- sales_model %>%
  group_by(lead_score_tier_xgb, primary_industry) %>%
  summarise(
    deal_count = n(),
    avg_deal = mean(deal_amount, na.rm = TRUE),
    total_sales = sum(deal_amount, na.rm = TRUE),
    avg_engagement = mean(engagement_score, na.rm = TRUE),
    contact_count = n_distinct(contact_id),
    .groups = "drop"
  ) %>%
  filter(deal_count >= 5) %>%
  arrange(desc(total_sales))

# Calculate relative contribution to total revenue
total_revenue <- sum(profile_summary$total_sales)
profile_summary <- profile_summary %>%
  mutate(
    revenue_share = total_sales / total_revenue,
    revenue_rank = row_number()
  )

# Save output
write_csv(profile_summary, here("outputs/tables", "market_segment_profiles.csv"))

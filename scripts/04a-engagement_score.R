# ------------------------------------------------------------
# Title: 04a-engagement_scoring.R
# Author: [Your Name]
# Date: 2025-04-30
# Description: Calculates engagement scores for all contacts based on activity data and assigns engagement tiers.
# Input: Contact activity dataset (contacts_unique.csv)
# Output: Scored engagement dataset with raw scores and tier levels
# Dependencies: tidyverse, here, janitor, lubridate, forcats
# ------------------------------------------------------------

library(here)
library(tidyverse)
library(janitor)
library(lubridate)
library(forcats)

# Load contact activity data
contacts <- read_csv(here("data/raw", "contacts_unique.csv")) %>%
  clean_names()

# Calculate derived engagement metrics
engagement_all <- contacts %>%
  select(
    contact_id,
    email,
    role,
    number_of_pageviews,
    number_of_sessions,
    number_of_form_submissions,
    marketing_emails_opened,
    marketing_emails_clicked,
    marketing_emails_replied,
    recent_sales_email_replied_date,
    currently_in_sequence,
    lifecycle_stage
  ) %>%
  mutate(
    recent_sales_email_replied = if_else(!is.na(recent_sales_email_replied_date), TRUE, FALSE),
    across(
      c(number_of_pageviews, number_of_sessions, number_of_form_submissions,
        marketing_emails_opened, marketing_emails_clicked, marketing_emails_replied),
      ~ replace_na(., 0)
    ),
    currently_in_sequence = replace_na(currently_in_sequence, FALSE),
    recent_sales_email_replied = replace_na(recent_sales_email_replied, FALSE),
    role = fct_na_value_to_level(as.factor(role), level = "unknown"),
    lifecycle_stage = fct_na_value_to_level(as.factor(lifecycle_stage), level = "unknown")
  ) %>%
  select(-recent_sales_email_replied_date)

# Calculate engagement score
engagement_all <- engagement_all %>%
  mutate(
    engagement_score_raw =
      0.2 * number_of_pageviews +
      0.2 * number_of_sessions +
      0.4 * number_of_form_submissions +
      0.5 * marketing_emails_opened +
      1.0 * marketing_emails_clicked +
      1.5 * marketing_emails_replied +
      2.0 * recent_sales_email_replied +
      1.0 * currently_in_sequence
  )

# Inspect score distribution
summary(engagement_all$engagement_score_raw)
hist(engagement_all$engagement_score_raw, breaks = 30,
     main = "Engagement Score Distribution", xlab = "Engagement Score")

# Assign engagement tiers
engagement_all <- engagement_all %>%
  mutate(
    engagement_tier = case_when(
      engagement_score_raw >= 15 ~ 5,
      engagement_score_raw >= 5 ~ 4,
      engagement_score_raw >= 1 ~ 3,
      engagement_score_raw > 0 ~ 2,
      TRUE ~ 1
    )
  )

# Review tier distribution
engagement_all %>%
  count(engagement_tier)

# Save processed engagement scores
write_csv(engagement_all, here("data/processed", "engagement_all.csv"))

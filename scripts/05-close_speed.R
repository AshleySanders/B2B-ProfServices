# ------------------------------------------------------------
# Title: 05-close_speed.R
# Author: Ashley Sanders
# Date: 2025-04-18
# Description: Analyzes deal close speed using survival analysis (Cox, AFT, RSF) to identify predictors of faster conversions.
# Input: all_deals_R.csv, all_contacts_all_columns.csv, companies_unique.csv
# Output: Model summaries, variable importance plot
# Dependencies: tidyverse, here, survival, survminer, ranger, broom, car, ggplot2
# ------------------------------------------------------------

library(here)
library(readr)
library(tidyverse)
library(lubridate)
library(janitor)
library(forcats)
library(survival)
library(survminer)
library(ranger)
library(broom)
library(car)
library(ggplot2)

# Load data
deals <- read_csv(here("data/raw", "all_deals_R.csv")) %>% clean_names()
contacts <- read_csv(here("data/raw", "all_contacts_all_columns.csv")) %>% clean_names()
companies <- read_csv(here("data/raw", "companies_unique.csv")) %>% clean_names()

# Remove duplicates and join
contacts_unique <- contacts %>% distinct(contact_id, .keep_all = TRUE)
company_a <- left_join(deals, contacts_unique, by = "contact_id", suffix = c("", ".right")) %>%
  select(-ends_with(".right"))

# Select and clean fields
company_a <- company_a %>%
  transmute(
    contact_id,
    primary_company_id,
    days_to_close = replace_na(as.numeric(days_to_close), 0),
    converted = as.numeric(closed_won_count),
    number_of_pageviews = replace_na(as.numeric(number_of_pageviews), 0),
    number_of_sessions = replace_na(as.numeric(number_of_sessions), 0),
    number_of_form_submissions = replace_na(as.numeric(number_of_form_submissions), 0),
    marketing_emails_opened = replace_na(as.numeric(marketing_emails_opened), 0),
    marketing_emails_clicked = replace_na(as.numeric(marketing_emails_clicked), 0),
    marketing_emails_replied = replace_na(as.numeric(marketing_emails_replied), 0),
    currently_in_sequence = if_else(currently_in_sequence %in% c(TRUE, "true"), 1, 0, missing = 0),
    recent_sales_email_replied = as.numeric(!is.na(recent_sales_email_replied_date)),
    lifecycle_stage = fct_na_value_to_level(as.factor(lifecycle_stage), level = "unknown"),
    role = fct_na_value_to_level(as.factor(role), level = "unknown"),
    job_domain = fct_na_value_to_level(as.factor(job_domain), level = "unknown"),
    number_of_times_contacted = replace_na(as.numeric(number_of_times_contacted), 0),
    number_of_sales_activities = replace_na(as.numeric(number_of_sales_activities), 0)
  )

# Recalculate engagement score
company_a <- company_a %>%
  mutate(
    engagement_score =
      0.2 * number_of_pageviews +
      0.2 * number_of_sessions +
      0.4 * number_of_form_submissions +
      0.5 * marketing_emails_opened +
      1.0 * marketing_emails_clicked +
      1.5 * marketing_emails_replied +
      2.0 * recent_sales_email_replied +
      1.0 * currently_in_sequence
  )

# Join with industry data
industry_data <- companies %>% select(primary_company_id, primary_industry)
company_a <- left_join(company_a, industry_data, by = "primary_company_id") %>%
  mutate(industry = fct_na_value_to_level(as.factor(primary_industry), level = "unknown")) %>%
  select(-primary_industry)

# Build survival object
days_to_close_fixed <- company_a %>%
  mutate(
    days_to_close_clean = case_when(
      is.na(days_to_close) ~ max(days_to_close, na.rm = TRUE),
      days_to_close == 0   ~ 1,
      TRUE ~ days_to_close
    ),
    converted_event = as.numeric(converted == 1)
  )

surv_obj <- Surv(time = days_to_close_fixed$days_to_close_clean, event = days_to_close_fixed$converted_event)

# Cox Hazards model shows signs of collinearity between engagement scores and either conversion or days_to_close, but we don't want to drop engagement scores, since our predictive models (using ranger and xgboost) demonstrated that it's a good predictor of conversion.

# Consequently, we'll use an Accelerated Failure Time (AFT) Model, which is parametric, models time directly, and can often handle correlated predictors better

# Fit AFT model
aft_model <- survreg(
  surv_obj ~ engagement_score + role + lifecycle_stage + number_of_times_contacted + industry,
  data = days_to_close_fixed,
  dist = "weibull"
)

aft_results <- tidy(aft_model) %>%
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ ".",
    TRUE ~ ""
  ))

print(aft_results, n = 43)

# Results were either inconclusive or simply reinforced best practices in sales and marketing.

# I'm going to try one more modeling technique to see if there is something meaningful that could help my client close deals faster.

# Fit Random Survival Forest
surv_obj_rsf <- Surv(time = days_to_close_fixed$days_to_close_clean, event = days_to_close_fixed$converted_event)

rsf_model <- ranger(
  formula = surv_obj_rsf ~ engagement_score + role + number_of_times_contacted + lifecycle_stage + industry,
  data = days_to_close_fixed,
  mtry = 3,
  num.trees = 1000,
  importance = "permutation",
  splitrule = "logrank",
  respect.unordered.factors = "partition"
)

importance_df <- data.frame(
  variable = names(rsf_model$variable.importance),
  importance = rsf_model$variable.importance
) %>%
  filter(!is.na(variable), !is.na(importance)) %>%
  arrange(desc(importance)) %>%
  slice_max(order_by = importance, n = 10)

# Plot top 10 variables by importance
ggplot(importance_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "#2C7BB6") +
  coord_flip() +
  labs(
    title = "Top Predictors of Deal Velocity (RSF)",
    x = "Predictor",
    y = "Importance (permutation error)"
  ) +
  theme_minimal +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

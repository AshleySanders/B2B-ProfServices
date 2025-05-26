# ------------------------------------------------------------
# Title: 07e-forecast_blended_and_segmented.R
# Author: Ashley Sanders
# Date: 2025-04-22
# Description: Combines Prophet and XGBoost forecasts using weighted averages and segments forecasts by industry and lead score tier.
# Input: prophet_forecast_comparison.csv, forecast_data.csv
# Output: blended_forecast.csv, forecast_summary_by_segment.csv
# Dependencies: tidyverse, here, lubridate, janitor
# ------------------------------------------------------------

library(here)
library(tidyverse)
library(lubridate)
library(janitor)

# Load combined forecast data for blending
forecast_df <- read_csv(here("data/processed", "prophet_forecast_comparison.csv")) %>%
  clean_names()

# Create blended forecast with adjustable weights
blended_forecast <- forecast_df %>%
  mutate(
    blended_weighted = 0.65 * xgb_yhat + 0.35 * prophet_yhat,
    blended_average = rowMeans(cbind(xgb_yhat, prophet_yhat), na.rm = TRUE)
  )

# Save blended forecast output
write_csv(blended_forecast, here("data/processed", "blended_forecast.csv"))

# Load detailed forecast data with contact-level info
forecast_data <- read_csv(here("data/processed", "forecast_data_scored.csv")) %>%
  clean_names()

# Calculate expected deal value using predicted XGBoost probability and amount
forecast_data <- forecast_data %>%
  mutate(expected_deal_value = predicted_deal_amount * predicted_prob_xgb)

# Segment forecast by close month, lead score tier, and primary industry
forecast_summary_by_segment <- forecast_data %>%
  mutate(close_month = floor_date(close_date_parsed, unit = "month")) %>%
  group_by(close_month, lead_score_tier_xgb, primary_industry) %>%
  summarise(
    n_deals = n(),
    total_expected_sales = sum(expected_deal_value, na.rm = TRUE),
    avg_expected_deal = mean(expected_deal_value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(close_month, desc(total_expected_sales))

# Save segmented forecast summary
write_csv(forecast_summary_by_segment, here("data/processed", "forecast_summary_by_segment.csv"))

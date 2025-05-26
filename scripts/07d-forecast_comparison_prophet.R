# ------------------------------------------------------------
# Title: 07d-forecast_comparison_prophet.R
# Author: Ashley Sanders
# Date: 2025-04-22
# Description: Fits a Prophet model to historical sales data and compares forecasts to XGBoost model outputs.
# Input: sales_model.csv, forecast_data_summary.csv
# Output: prophet_forecast_comparison.csv
# Dependencies: tidyverse, here, lubridate, prophet
# ------------------------------------------------------------

library(here)
library(tidyverse)
library(lubridate)
library(prophet)

# Load historical sales data for Prophet model
sales_model <- read_csv(here("data/processed", "sales_model.csv")) %>% clean_names()

# Prepare data for Prophet: rename and group
prophet_df <- sales_model %>%
  mutate(ds = close_month, y = deal_amount) %>%
  group_by(ds) %>%
  summarise(y = sum(y, na.rm = TRUE), .groups = "drop") %>%
  arrange(ds)

# Fit Prophet model
prophet_model <- prophet(prophet_df, yearly.seasonality = TRUE, weekly.seasonality = FALSE, daily.seasonality = FALSE)

# Create future dataframe for same months in forecast
future_months <- read_csv(here("data/processed", "forecast_data_summary.csv")) %>%
  clean_names() %>%
  rename(ds = close_month) %>%
  select(ds) %>%
  distinct()

# Forecast with Prophet
prophet_forecast <- predict(prophet_model, future_months)

# Extract relevant columns and combine with XGBoost
prophet_out <- prophet_forecast %>%
  select(ds, prophet_yhat = yhat, prophet_lower = yhat_lower, prophet_upper = yhat_upper)

xgb_forecast <- read_csv(here("data/processed", "forecast_data_summary.csv")) %>%
  rename(xgb_yhat = total_predicted_sales) %>%
  select(ds = close_month, xgb_yhat)

# Combine forecasts for comparison
comparison_df <- left_join(xgb_forecast, prophet_out, by = "ds")

# Save combined output
write_csv(comparison_df, here("data/processed", "prophet_forecast_comparison.csv"))

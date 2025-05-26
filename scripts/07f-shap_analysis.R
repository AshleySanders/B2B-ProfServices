# ------------------------------------------------------------
# Title: 07f-shap_analysis.R
# Author: Ashley Sanders
# Date: 2025-04-22
# Description: Explains predictions from the XGBoost model using SHAP values and highlights key drivers of forecasted deal value.
# Input: xgb_model.rds, sales_model.csv
# Output: SHAP summary plot and feature importance table
# Dependencies: tidyverse, here, xgboost, iml, Matrix, janitor
# ------------------------------------------------------------

library(here)
library(tidyverse)
library(xgboost)
library(Matrix)
library(janitor)
library(iml)

# Load data and model
sales_model <- read_csv(here("data/processed", "sales_model.csv")) %>% clean_names()
xgb_model <- readRDS(here("data/processed", "xgb_model.rds"))

# Prepare feature matrix
x_matrix <- model.matrix(deal_amount ~ . -deal_id -close_date_parsed -close_month, data = sales_model)[, -1]
y_vector <- sales_model$deal_amount

# Convert to XGBoost DMatrix
xgb_matrix <- xgb.DMatrix(data = x_matrix, label = y_vector)

# Create Predictor object
predictor <- Predictor$new(
  model = xgb_model$finalModel,
  data = as.data.frame(x_matrix),
  y = y_vector
)

# Compute SHAP values
shap <- Shapley$new(predictor, sample.size = 100)

# Plot feature importance
plot(shap)

# Optional: aggregate feature importances
shap_values <- shap$results %>%
  group_by(feature) %>%
  summarise(mean_phi = mean(abs(phi), na.rm = TRUE)) %>%
  arrange(desc(mean_phi))

print(shap_values)

# Save feature importance table
write_csv(shap_values, here("data/processed", "shap_feature_importance.csv"))

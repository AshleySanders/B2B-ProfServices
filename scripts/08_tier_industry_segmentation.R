# ------------------------------------------------------------
# Title: 08-tier_industry_segmentation.R
# Author: [Your Name]
# Date: 2025-04-30
# Description: Analyzes historical performance by lead score tier and industry using Kruskal-Wallis tests, Cliff's Delta, and visualizations.
# Input: sales_model.csv
# Output: Summary tables and visualizations by tier and industry
# Dependencies: tidyverse, here, janitor, ggplot2, car, effsize, rstatix
# ------------------------------------------------------------

library(here)
library(tidyverse)
library(janitor)
library(ggplot2)
library(car)
library(effsize)
library(rstatix)

# Load data
sales_model <- read_csv(here("data/processed", "sales_model.csv")) %>% clean_names()

# Summary stats by lead score tier and industry
tier_summary <- sales_model %>%
  group_by(lead_score_tier_xgb) %>%
  summarise(
    deal_count = n(),
    total_sales = sum(deal_amount, na.rm = TRUE),
    avg_deal = mean(deal_amount, na.rm = TRUE),
    .groups = "drop"
  )

industry_summary <- sales_model %>%
  group_by(primary_industry) %>%
  summarise(
    deal_count = n(),
    total_sales = sum(deal_amount, na.rm = TRUE),
    avg_deal = mean(deal_amount, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_sales))

# Check normality and homogeneity
shapiro_test_res <- sales_model %>%
  group_by(lead_score_tier_xgb) %>%
  summarise(p_value = shapiro_test(deal_amount)$p)

levene_result <- leveneTest(deal_amount ~ lead_score_tier_xgb, data = sales_model)
print(shapiro_test_res)
print(levene_result)

# Kruskal-Wallis tests
kruskal_tier <- kruskal_test(deal_amount ~ lead_score_tier_xgb, data = sales_model)
kruskal_industry <- kruskal_test(deal_amount ~ primary_industry, data = sales_model)

print(kruskal_tier)
print(kruskal_industry)

# Effect size: Cliff's Delta
cliffs_delta_result <- sales_model %>%
  filter(lead_score_tier_xgb %in% c("High", "Medium", "Low")) %>%
  mutate(tier_num = case_when(
    lead_score_tier_xgb == "High" ~ 3,
    lead_score_tier_xgb == "Medium" ~ 2,
    TRUE ~ 1
  )) %>%
  cliff.delta(deal_amount ~ tier_num, data = .)

print(cliffs_delta_result)

# Boxplots for visual inspection
sales_model %>%
  ggplot(aes(x = lead_score_tier_xgb, y = deal_amount, fill = lead_score_tier_xgb)) +
  geom_boxplot() +
  labs(
    title = "Deal Amount by Lead Score Tier",
    x = "Lead Score Tier",
    y = "Deal Amount"
  ) +
  theme_minimal()

sales_model %>%
  filter(primary_industry %in% names(sort(table(primary_industry), decreasing = TRUE)[1:6])) %>%
  ggplot(aes(x = fct_reorder(primary_industry, deal_amount, .fun = median), y = deal_amount)) +
  geom_boxplot(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Deal Amount by Industry (Top 6)",
    x = "Industry",
    y = "Deal Amount"
  ) +
  theme_minimal()

# Save outputs
write_csv(tier_summary, here("outputs/tables", "tier_summary.csv"))
write_csv(industry_summary, here("outputs/tables", "industry_summary.csv"))
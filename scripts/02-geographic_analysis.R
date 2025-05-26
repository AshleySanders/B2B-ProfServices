# ------------------------------------------------------------
# Title: 02-geographic_analysis.R
# Author: Ashley Sanders
# Date: 2025-04-30
# Description: Geographic comparison of conversion performance and deal behavior by continent.
# Input: Processed fulljoin dataset (coA_fulljoin.csv)
# Output: Summary statistics, effect sizes, Dunn test results, and visualizations
# Dependencies: dplyr, ggplot2, pheatmap, FSA, rcompanion, effsize
# ------------------------------------------------------------

library(here)
library(readr)
library(tidyverse)
library(stringr)
library(ggplot2)
library(pheatmap)
library(FSA)
library(ggalt)
library(dunn.test)
library(rcompanion)
library(effsize)

# Load data
data <- read_csv(here("data_clean", "coA_fulljoin.csv"))

# Clean and filter
geo_data <- data %>%
  filter(!is.na(continent),
         !is.na(deal_amount),
         !is.na(closed_won_count),
         !is.na(days_to_close)) %>%
  mutate(
    deal_amount = as.numeric(deal_amount),
    closed_won_count = as.numeric(closed_won_count),
    days_to_close = as.numeric(days_to_close)
  )

# Summarize by continent
region_summary <- geo_data %>%
  group_by(continent) %>%
  summarise(
    total_deals = n(),
    closed_deals = sum(closed_won_count),
    avg_deal_size = mean(deal_amount, na.rm = TRUE),
    avg_days_to_close = mean(days_to_close, na.rm = TRUE),
    conversion_rate = closed_deals / total_deals,
    .groups = "drop"
  )

# Non-parametric tests due to non-normal distributions
kruskal.test(deal_amount ~ continent, data = geo_data)
kruskal.test(closed_won_count ~ continent, data = geo_data)
kruskal.test(days_to_close ~ continent, data = geo_data)

# Boxplots
geo_data %>%
  ggplot(aes(x = continent, y = deal_amount)) +
  geom_boxplot() +
  labs(title = "Deal Amount by Continent")

geo_data %>%
  ggplot(aes(x = continent, y = days_to_close)) +
  geom_boxplot() +
  labs(title = "Days to Close by Continent")

geo_data %>%
  ggplot(aes(x = continent, y = closed_won_count)) +
  geom_boxplot() +
  labs(title = "Closed Deals Count by Continent")

# Eta-Squared Calculation
kwEtaSq <- function(kw_test) {
  H <- kw_test$statistic
  N <- sum(kw_test$parameter + 1)
  eta_sq <- as.numeric(H / (N - 1))
  return(eta_sq)
}

eta_sq_deal <- kwEtaSq(kruskal.test(deal_amount ~ continent, data = geo_data))
eta_sq_days <- kwEtaSq(kruskal.test(days_to_close ~ continent, data = geo_data))

cat("Eta-squared for Deal Amount:", eta_sq_deal, "\n")
cat("Eta-squared for Days to Close:", eta_sq_days, "\n")

# Dunn's Test post-hoc comparisons
dunnTest(deal_amount ~ continent, data = geo_data, method = "bonferroni")
dunnTest(days_to_close ~ continent, data = geo_data, method = "bonferroni")
dunnTest(closed_won_count ~ continent, data = geo_data, method = "bonferroni")

# Cliff's Delta Effect Size
get_pairwise_cliffs <- function(data, metric, group_var) {
  group_levels <- unique(data[[group_var]])
  results <- data.frame(group1 = character(), group2 = character(), delta = numeric(), magnitude = character(), stringsAsFactors = FALSE)

  for (i in 1:(length(group_levels) - 1)) {
    for (j in (i + 1):length(group_levels)) {
      g1 <- group_levels[i]
      g2 <- group_levels[j]

      data1 <- data[[metric]][data[[group_var]] == g1]
      data2 <- data[[metric]][data[[group_var]] == g2]

      test <- cliff.delta(data1, data2)

      results <- rbind(results, data.frame(
        group1 = g1,
        group2 = g2,
        delta = test$estimate,
        magnitude = test$magnitude
      ))
    }
  }
  return(results)
}

cliffs_deal <- get_pairwise_cliffs(geo_data, "deal_amount", "continent")
cliffs_days <- get_pairwise_cliffs(geo_data, "days_to_close", "continent")

# Final Summary Table
summary_table <- geo_data %>%
  group_by(continent) %>%
  summarise(
    avg_deal_amount = mean(deal_amount, na.rm = TRUE),
    avg_days_to_close = mean(days_to_close, na.rm = TRUE),
    closed_deal_count = sum(closed_won_count, na.rm = TRUE),
    .groups = "drop"
  )

# Save summary table
write_csv(summary_table, here("outputs/tables", "geographic_summary_by_continent.csv"))
write_csv(cliffs_deal, here("outputs/tables", "cliffs_delta_deal_amount.csv"))
write_csv(cliffs_days, here("outputs/tables", "cliffs_delta_days_to_close.csv"))

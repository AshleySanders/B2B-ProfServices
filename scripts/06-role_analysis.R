# ------------------------------------------------------------
# Title: 06-role_analysis.R
# Author: Ashley Sanders
# Date: 2025-04-21
# Description: Compares engagement across roles and visualizes funnel drop-off using statistical tests and funnel plots.
# Input: engagement_all.csv, all_deals_R.csv
# Output: Summary tables, Fisher's test results, funnel visualizations
# Dependencies: tidyverse, here, janitor, ggplot2, scales
# ------------------------------------------------------------

library(here)
library(readr)
library(tidyverse)
library(janitor)
library(ggplot2)
library(scales)

# Load data
engagement <- read_csv(here("data/processed", "engagement_all.csv")) %>% clean_names()
deals <- read_csv(here("data/raw", "all_deals_R.csv")) %>% clean_names()

# Unique engagement records
deals_contacts <- left_join(deals, engagement, by = "contact_id") %>% clean_names()

# Group summary by role
engagement_by_role <- engagement %>%
  group_by(role) %>%
  summarise(across(c(engagement_score_raw:number_of_sales_activities), mean, na.rm = TRUE), contact_count = n()) %>%
  arrange(desc(engagement_score_raw))

print(engagement_by_role)

# Compare engaged contacts with and without deals
engaged_contacts_all <- engagement %>%
  filter(engagement_score_raw > 0) %>%
  count(role, name = "n_all") %>%
  mutate(percent_all = n_all / sum(n_all))

engaged_contacts_deals <- deals_contacts %>%
  filter(engagement_score_raw > 0) %>%
  count(role, name = "n_deals") %>%
  mutate(percent_deals = n_deals / sum(n_deals))

role_comparison <- full_join(engaged_contacts_all, engaged_contacts_deals, by = "role") %>%
  replace_na(list(n_all = 0, percent_all = 0, n_deals = 0, percent_deals = 0)) %>%
  mutate(
    percent_diff = percent_deals - percent_all,
    percent_diff_label = percent(percent_diff, accuracy = 0.1)
  ) %>%
  arrange(desc(abs(percent_diff)))

print(role_comparison)

# Plot role differences
ggplot(role_comparison, aes(x = reorder(role, percent_diff), y = percent_diff)) +
  geom_col(aes(fill = percent_diff > 0)) +
  geom_text(aes(label = percent_diff_label), hjust = ifelse(role_comparison$percent_diff > 0, -0.1, 1.1)) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Role Representation: Engaged Contacts vs Deals",
    subtitle = "Positive = Overrepresented in deals; Negative = Drop-off from initial engagement",
    x = "Role",
    y = "Difference in Proportion"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# Statistical test
role_counts <- bind_rows(
  engagement %>% filter(engagement_score_raw > 0, !is.na(role)) %>% mutate(group = "All_Engaged") %>% select(role, group),
  deals_contacts %>% filter(engagement_score_raw > 0, !is.na(role)) %>% mutate(group = "Deals_Engaged") %>% select(role, group)
) %>%
  count(group, role) %>%
  pivot_wider(names_from = group, values_from = n, values_fill = 0) %>%
  column_to_rownames("role")

fisher_test <- fisher.test(role_counts, simulate.p.value = TRUE)
print(fisher_test)

# Post-hoc role-level tests
role_long <- role_counts %>%
  rownames_to_column("role") %>%
  pivot_longer(cols = everything(), names_to = "group", values_to = "count") %>%
  pivot_wider(names_from = group, values_from = count)

posthoc_results <- purrr::map_dfr(1:nrow(role_long), function(i) {
  row <- role_long[i, ]
  mat <- matrix(c(row$Deals_Engaged, sum(role_long$Deals_Engaged) - row$Deals_Engaged,
                  row$All_Engaged, sum(role_long$All_Engaged) - row$All_Engaged),
                nrow = 2, byrow = TRUE)
  tibble(role = row$role, p_value = fisher.test(mat)$p.value)
}) %>%
  mutate(p_adj = p.adjust(p_value, method = "holm"))

role_comparison_sig <- role_comparison %>%
  left_join(posthoc_results, by = "role") %>%
  arrange(desc(abs(percent_diff)))

write_csv(role_comparison_sig, here("outputs/tables", "role_comparison_sig.csv"))

# Funnel plot prep
role_funnel <- role_comparison %>%
  select(role, n_all, percent_all, percent_deals) %>%
  filter(!is.na(role), n_all >= 100) %>%
  arrange(desc(percent_all)) %>%
  mutate(y = row_number())

polygon_data <- role_funnel %>%
  mutate(
    x_left_top = -percent_all / 2,
    x_right_top = percent_all / 2,
    x_right_bottom = percent_deals / 2,
    x_left_bottom = -percent_deals / 2,
    y_top = y + 0.4,
    y_bottom = y - 0.4
  ) %>%
  rowwise() %>%
  mutate(x = list(c(x_left_top, x_right_top, x_right_bottom, x_left_bottom)), y = list(c(y_top, y_top, y_bottom, y_bottom))) %>%
  unnest(c(x, y))

annotations <- tibble(x = c(0, 0), y = c(max(role_funnel$y) + 0.8, min(role_funnel$y) - 0.8), label = c("All Engaged Contacts", "Contacts with Deals"))

ggplot(polygon_data, aes(x = x, y = y, group = role, fill = role)) +
  geom_polygon(color = "white", alpha = 0.9) +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(breaks = role_funnel$y, labels = role_funnel$role, expand = expansion(add = 0.5)) +
  scale_x_continuous(labels = percent_format()) +
  geom_text(data = annotations, aes(x = x, y = y, label = label), inherit.aes = FALSE, fontface = "bold", size = 4.2) +
  labs(title = "Funnel of Engagement by Role", subtitle = "Drop-off from All Engaged Contacts to Contacts with Deals") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "none"
  )

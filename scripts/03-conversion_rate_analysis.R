# ------------------------------------------------------------
# Title: 03-conversion_rate_analysis.R
# Author: Ashley Sanders
# Date: 2025-04-30
# Description: Visualizes conversion rate differences across job roles and domains using an interactive mosaic plot.
# Input: Processed fulljoin dataset (coA_fulljoin.csv)
# Output: Interactive HTML mosaic plot
# Dependencies: tidyverse, ggplot2, ggiraph, htmlwidgets
# ------------------------------------------------------------

library(tidyverse)
library(scales)
library(ggplot2)
library(ggiraph)
library(htmlwidgets)
library(here)

# Load data
company_data <- read_csv(here("data/processed", "coA_fulljoin.csv"))

#######################
## Conversion Rates ##
#######################

conversion_data <- company_data %>%
  filter(!is.na(role), !is.na(job_domain))

# Summarize by role and domain
summary_df <- conversion_data %>%
  group_by(role, job_domain) %>%
  summarise(
    total_deals = n(),
    closed_deals = sum(closed_won_count),
    conversion_rate = closed_deals / total_deals,
    .groups = 'drop'
  )

# Role totals for width scaling
role_totals <- summary_df %>%
  group_by(role) %>%
  summarise(
    role_total_deals = sum(total_deals),
    .groups = 'drop'
  ) %>%
  mutate(
    role_width = role_total_deals / sum(role_total_deals)
  )

# Combine data
mosaic_data <- conversion_data %>%
  group_by(role, job_domain) %>%
  summarise(
    n_deals = n(),
    n_closed = sum(closed_won_count, na.rm = TRUE),
    conversion_rate = n_closed / n_deals,
    .groups = "drop"
  ) %>%
  group_by(role) %>%
  mutate(
    total_deals_role = sum(n_deals),
    domain_prop = n_deals / total_deals_role
  ) %>%
  ungroup()

# Calculate widths for x-axis blocks
role_widths <- mosaic_data %>%
  group_by(role) %>%
  summarise(width = sum(n_deals), .groups = "drop") %>%
  mutate(width = width / sum(width)) %>%
  arrange(desc(width)) %>%
  mutate(
    xmin = lag(cumsum(width), default = 0),
    xmax = xmin + width
  )

# Merge width and domain proportions
mosaic_data <- mosaic_data %>%
  left_join(role_widths, by = "role") %>%
  group_by(role) %>%
  arrange(job_domain, .by_group = TRUE) %>%
  mutate(
    ymin = lag(cumsum(domain_prop), default = 0),
    ymax = ymin + domain_prop
  ) %>%
  ungroup() %>%
  mutate(
    tooltip_text = paste0(
      "Role: ", role, "\n",
      "Domain: ", job_domain, "\n",
      "Deals: ", n_deals, "\n",
      "Conversion Rate: ", round(conversion_rate * 100, 1), "%"
    )
  )

# Plot
p <- ggplot(mosaic_data) +
  geom_rect_interactive(
    aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax,
      fill = conversion_rate,
      tooltip = tooltip_text
    ),
    color = "white"
  ) +
  scale_fill_viridis_c(name = "Conversion Rate", labels = percent_format(accuracy = 1)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "right")

# Interactive export
mosaic_girafe <- girafe(
  ggobj = p,
  width_svg = 10,
  height_svg = 6,
  options = list(
    opts_tooltip(opacity = 0.95, css = "padding:6px; background-color:#333; color:white; border-radius:4px;"),
    opts_hover(css = "stroke:black;stroke-width:2px;")
  )
)

# Save output
saveWidget(mosaic_girafe, file = here("outputs", "conversion_mosaic_plot.html"), selfcontained = TRUE)
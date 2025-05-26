# Title: Company A Data Cleaning and Preparation
# Author: Ashley Sanders
# Date: April 6, 2025
# Description: Clean and prepare raw CRM, contact, and company data.
# Input: Raw CSVs from HubSpot and ZoomInfo
# Output: Cleaned contact, deal, and company datasets

library(here)
library(readr)
library(tidyverse)
library(janitor)
library(ggplot2)
library(stringr)
here()

deals <- read_csv(here("data/raw", "all_deals.csv"))


## DATA CLEANING & PREPARATION ##

# Clean column names (snake_case)
deals <- deals %>% clean_names()

# View summary of missing values
missing_summary <- deals %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "missing_pct") %>%
  arrange(desc(missing_pct))

print(missing_summary, n = 186)

# Remove columns with more than 99% missing values
cols_to_remove <- deals %>% select(where(~ mean(is.na(.)) > 0.999))

deals <- deals %>% select(-all_of(colnames(cols_to_remove)))

# Trim whitespace from all character columns
deals <- deals %>% mutate(across(where(is.character), str_trim))

# Print clean dataset info
glimpse(deals)

write_csv(deals, here("data/processed", "all_deals_R.csv"))

##############################
## Prepare Contacts dataset ##
##############################

contacts <- read_csv(here("data/processed", "all_contacts.csv"))

# Clean column names using snake_case
contacts <- contacts %>% clean_names()

# Trim whitespace from all character columns
contacts <- contacts %>% mutate(across(where(is.character), str_trim))

# Remove duplicate rows
contacts <- contacts %>% distinct()


# Function to simplify job titles
simplify_role <- function(title) {
  title <- tolower(title)

  case_when(
    is.na(title) ~ NA_character_,
    str_detect(title, "ceo|chief executive officer|founder|owner|president") ~ "CEO",
    str_detect(title, "managing director") ~ "Managing Director",
    str_detect(title, "executive director") ~ "Executive Director",
    str_detect(title, "vp|vice president") ~ "VP",
    str_detect(title, "\\bdirector\\b") ~ "Director",
    str_detect(title, "manager") ~ "Manager",
    str_detect(title, "coo|chief operating officer") ~ "COO",
    str_detect(title, "cfo|chief financial officer") ~ "CFO",
    str_detect(title, "cmo|chief marketing officer") ~ "CMO",
    str_detect(title, "chief revenue officer|cro") ~ "CRO",
    str_detect(title, "marketing director|director of marketing") ~ "Marketing Director",
    str_detect(title, "lead|head") ~ "Lead",
    str_detect(title, "engineer|developer|technician") ~ "Engineer",
    str_detect(title, "consultant|advisor|analyst") ~ "Consultant",
    str_detect(title, "cto|chief technology officer|vp of engineering|vice president of engineering") ~ "CTO",
    str_detect(title, "intern") ~ "Intern",
    TRUE ~ "Other"
  )
}

# Function to determine domain
determine_domain <- function(title) {
  title <- tolower(title)

  case_when(
    is.na(title) ~ NA_character_,
    str_detect(title, "marketing director|director of marketing") ~ "Marketing",
    str_detect(title, "ceo|chief executive officer|founder|president|owner|managing director|executive director") ~ "Executive",
    str_detect(title, "cmo|chief marketing officer|marketing|brand|growth|seo|content") ~ "Marketing",
    str_detect(title, "sales|account executive|business development|revenue|cro") ~ "Sales",
    str_detect(title, "finance|accounting|cfo|controller|bookkeeper") ~ "Finance",
    str_detect(title, "operations|coo|logistics|supply chain") ~ "Operations",
    str_detect(title, "product|ux|ui|design|designer") ~ "Product",
    str_detect(title, "human resources|hr|people|recruiter") ~ "HR",
    str_detect(title, "cto|engineer|developer|technician|technical") ~ "Engineering",
    str_detect(title, "customer success|support|client success|service") ~ "Customer Success",
    str_detect(title, "legal|attorney|lawyer|counsel") ~ "Legal",
    TRUE ~ "Other"
  )
}

# Apply the functions
contacts <- contacts %>%
  mutate(
    role = simplify_role(job_title_original),
    job_domain = determine_domain(job_title_original)
  )

##################################
## Prepare the company data set ##
##################################

co_zoominfo <- read_csv(here("data/raw", "co_zoom_data.csv"))

crm_co <- read_csv(here("data/raw", "company_list.csv"))

co_zoominfo <- co_zoominfo %>% clean_names()
crm_co <- crm_co %>% clean_names()

co_zoominfo <- co_zoominfo %>% distinct()
crm_co <- crm_co %>% distinct()

# Trim whitespace from all character columns
co_zoominfo <- co_zoominfo %>% mutate(across(where(is.character), str_trim))
crm_co <- crm_co %>% mutate(across(where(is.character), str_trim))

colnames(co_zoominfo)
colnames(crm_co)

# Company names were too messy to fully reconcile.
# Check company website urls in ZoomInfo and HubSpot (CRM) lists to see if they can be used as a primary key for the join

normalize_url <- function(url) {
  url <- tolower(url)
  url <- str_trim(url)
  url <- str_replace_all(url, "^https?://", "")  # remove http/https
  url <- str_replace(url, "^www\\.", "")         # remove www.
  url <- str_replace(url, "/$", "")              # remove trailing slash
  return(url)
}

# co_zoominfo
# crm_co

# Normalize website fields
crm_df <- crm_co %>%
  mutate(website_norm = normalize_url(website_url))

zoominfo_df <- co_zoominfo %>%
  mutate(website_norm = normalize_url(website))

# Join on normalized website
joined_df <- left_join(zoominfo_df, crm_df, by = "website_norm")

write_csv(joined_df, here("data/raw", "joined_company_data.csv"))

# Check to see for how many companies we were able to match website urls and link the primary_company_id.
sum(is.na(joined_df$primary_company_id))
sum(!is.na(joined_df$primary_company_id))
sum(is.na(crm_co$primary_company_id))
sum(!is.na(crm_co$primary_company_id))

# I have primary_company_ids for 89% of the companies whose information exists in both the ZoomInfo list and the CRM.

# After the join, I removed unnecessary columns, reconciled country names, and added a column for continents in Excel and Open Refine.

# Uploading the refined dataset:
companies <- read_csv(here("data/processed", "all_companies_simp.csv"))

# Clean and prepare data set to join with deals
companies <- companies %>% clean_names()

table(table(companies$primary_company_id))  # Only shows counts

# Check how many rows per company ID
dupes <- companies %>%
  group_by(primary_company_id) %>%
  filter(n() > 1)

nrow(dupes)  # This should be 0 ideally

companies_unique <- companies %>%
  distinct(primary_company_id, .keep_all = TRUE)

# Create a joined data between companies_unique and deals
joined_deals <- left_join(deals, companies_unique, by = "primary_company_id", relationship = "many-to-one")


# Prepare contacts dataset for join with the joined_deals dataset based on contact_id.

table(table(contacts$contact_id))  # Only shows counts

# Check how many rows per company ID
dupes <- contacts %>%
  group_by(contact_id) %>%
  filter(n() > 1)

nrow(dupes)  # This should be 0 ideally

contacts_unique <- contacts %>%
  distinct(contact_id, .keep_all = TRUE)

dupes <- contacts_unique %>%
group_by(contact_id) %>%
filter(n() > 1)
nrow(dupes)  # This should be 0 ideally


co_fulljoin <- left_join(joined_deals, contacts_unique, by="contact_id", relationship = "many-to-one")

# Check accuracy of the final join:

nrow(co_fulljoin) == nrow(deals)  # Should be TRUE
table(co_fulljoin$closed_won_count)  # Should be 748 / 336

co_fulljoin <- co_fulljoin %>% clean_names()

write_csv(co_fulljoin, here("data/processed", "coA_fulljoin.csv"))

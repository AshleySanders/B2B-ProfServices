# Company\_A Data Analysis & Sales Forecasting Project

This repository contains a comprehensive, modular data science pipeline developed to analyze historical sales data, model lead engagement and conversion, forecast future revenue, and segment high-performing market profiles.

The project has been anonymized for sharing purposes, with the client referred to as **Company\_A**.

---

## Project Overview

The project supports strategic sales decision-making by:

* Calculating contact engagement and lead scores
* Predicting lead conversion probability
* Forecasting short-term deal value with machine learning and time series models
* Segmenting historical and forecasted performance by tier, industry, geography, and role

---

## Project Structure

```
├── data/
│   ├── raw/                  # Raw source data files (deals, contacts, companies)
│   └── processed/            # Cleaned and transformed data for modeling
├── outputs/
│   └── tables/               # Summary outputs (CSV tables, stats, profiles)
├── scripts/                  # Cleaned, modular R scripts
└── README.md                 # Project documentation
```

---

## Data Cleaning & Transformation

The following steps were used to clean and prepare data:

* Removed duplicate rows across deals, contacts, and companies
* Standardized and renamed columns using `janitor::clean_names()`
* Parsed and corrected date fields using `lubridate`
* Converted roles, industries, and lifecycle stages into factor variables with fallback values (e.g., "unknown")
* Engineered features like `engagement_score`, `deal_age`, and `lead_score_tier`
* Filled missing numeric fields with 0 where appropriate (e.g., email activity, web sessions)
* **Continent information** was added manually based on country of the primary company

All cleaned outputs were written to `data/processed/` and serve as the base for modeling and forecasting.

---

## Pipeline Summary

### Phase 1: Data Cleaning & Preparation

| Script                          | Description                                             |
| ------------------------------- | ------------------------------------------------------- |
| `01-data_cleaning.R`            | Cleans and merges deals, contacts, and company records  |
| `02-geographic_analysis.R`      | Analyzes conversion rate and close speed by continent   |
| `03-conversion_rate_analysis.R` | Mosaic plot of conversion rate by job role and domain   |
| `04a-engagement_scoring.R`      | Calculates contact engagement scores from activity data |

### Phase 2: Lead Scoring & Conversion Modeling

| Script                             | Description                                                 |
| ---------------------------------- | ----------------------------------------------------------- |
| `04b-lead_model_logit_rf.R`        | Logistic regression and random forest conversion prediction |
| `04c-lead_model_xgb.R`             | XGBoost lead conversion model and tiering                   |
| `04d-apply_scoring_all_contacts.R` | Applies conversion model to all contacts                    |

### Phase 3: Deal Value Forecasting

| Script                                 | Description                                            |
| -------------------------------------- | ------------------------------------------------------ |
| `07a-data_preparation.R`               | Prepares historical deal data for regression modeling  |
| `07b-model_training_xgb.R`             | Trains XGBoost model to predict deal amount            |
| `07c-forecast_generation.R`            | Applies XGBoost to open deals to forecast future sales |
| `07d-forecast_comparison_prophet.R`    | Prophet forecast + comparison with ML model            |
| `07e-forecast_blended_and_segmented.R` | Blended forecast and segmentation by tier/industry     |
| `07f-shap_analysis.R`                  | SHAP-based interpretation of XGBoost forecasts         |

### Phase 4: Segmentation & Market Profiling

| Script                            | Description                                                          |
| --------------------------------- | -------------------------------------------------------------------- |
| `08-tier_industry_segmentation.R` | Statistical comparisons and visualizations by lead tier and industry |
| `09-market_summary_metrics.R`     | Summarizes deal volume, value, and engagement by segment             |
| `10-market_profiles.R`            | Builds ranked segment profiles for strategic targeting               |

---

## Key Outputs

* **Lead score tiers**: Assigned based on XGBoost probability bins
* **Engagement scoring**: Weighted mix of activity-based indicators
* **Forecasts**: Future deal value by month, segment, and blend of Prophet + ML
* **Market profiles**: Ranked industry + tier segments by volume and revenue

---

## Technologies Used

* **Language**: R (caret, xgboost, tidyverse, prophet, iml, janitor, here)
* **Modeling**: Logistic Regression, Random Forest, XGBoost, Prophet
* **Explainability**: SHAP (via `iml`)
* **Statistics**: Kruskal-Wallis, Cliff’s Delta, Dunn's test, Fisher's test

---

## Notes

* All file paths use `here::here()` for portability
* All outputs are saved in clearly labeled folders
* Raw data is excluded from version control but structure is documented

---

## Contact

For questions about this project or to reuse this pipeline for your own business analysis:
**\Ashley Sanders**
Data Scientist, Sanders Analytics
\https://sandersanalytics.org
\https://www.linkedin.com/in/ashleyrsanders/

---

*This project was anonymized and refactored for public sharing. Proprietary identifiers have been removed or aliased.*

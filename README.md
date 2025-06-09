# measles-importation-risk

This repository contains code for predicting the **probability of measles case occurrence** at the county level within a **1–3 week horizon**. The model is built using **gradient boosting (XGBoost)** and uses publicly available surveillance and demographic data.

## Objective

Estimate the **weekly risk of measles outbreak** at the county level in Texas using:

- Case-based surveillance (linelist) data 
- Vaccination coverage
- Demographic and age-structured population data
- Mobility flows between counties
- Distance to recent outbreak hotspots
- Mennonite community presence
- Rolling incidence and temporal lags


## Features Used

- Vaccination rate (latest available)
- % of population in age groups (0–4, 5–9, 10–17, 18-64, 65+)
- Total population size
- Number of neighboring counties
- Incidence in neighboring counties 
- Weekly mobility flows from major outbreak counties
- Distance from outbreak counties
- Lagged incidence (1–3 weeks)
- 3-week rolling mean of incidence
- Mennonite presence (binary)

## Target Variable

We model the **probability** of at least one new measles case in county *i* during a given week.

Binary target:
- `1`: New case occurs in county *i* in that week
- `0`: No new case

## Modeling Approach

- **XGBoost** (`binary:logistic`) is used to estimate probabilities
- **Imbalanced data** is addressed using `scale_pos_weight`
- **Train/test split**: First 3–4 months for training, last 1 month for testing
- Spatial smoothing and risk scoring evaluated post-model

## Visualization

Maps and GIFs are generated using `ggplot2`, `gganimate`, and `sf` to show spatial risk dynamics over time.

## Repository Structure



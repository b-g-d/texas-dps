# Texas DPS Traffic Stop Analysis

Analysis of racial disparities in vehicle searches during Texas Department of Public Safety traffic stops, using officer-level stop data.

## Overview

This project examines whether officers in the Texas DPS search drivers of different racial backgrounds at different rates. Two complementary approaches are used:

1. **Descriptive comparison** — per-officer search rates for white vs. non-white drivers, with confidence intervals, broken down by region, district, and service type.
2. **Logistic regression modeling** — per-officer binomial GLMs that control for confounders (driver sex, age, state of licensure, vehicle age) to isolate the effect of race on the probability of being searched.

## Repository Structure

```
texas-dps/
├── search_by_officer.r              # Compare white vs. non-white search rates per officer (scatter plots)
├── search_by_officer_location.R     # Search rate comparisons broken out by region/district/service
├── modeling_search_probability.R    # Per-officer logistic regression models
├── officer_search_by_region.png     # Output: search rate visualization by region
└── data/                            # Input CSVs (not committed)
    ├── groupby_officerid_race.csv
    ├── officer_service_race_search.csv
    ├── search_rates_by_officer_race.csv
    └── regression_features/
        └── regression_features_select_officers.csv
```

Results are written to a `results/` directory (not committed):
- `results/officer_models.txt` — summary output for all per-officer logistic regressions
- `results/comp_search_rates.csv` — ranked table of white/non-white search rate differences per officer

## Scripts

### `search_by_officer.r`

Reads `data/groupby_officerid_race.csv` and computes 99.5% confidence intervals for each officer's search rate separately for white and non-white drivers. Produces scatter plots (non-white rate vs. white rate) with a 45° equality line — points above the line indicate officers who search non-white drivers at a higher rate.

**Packages:** `ggplot2`, `tidyr`

### `search_by_officer_location.R`

Reads `data/officer_service_race_search.csv`, merges white/non-white records per officer, and computes the raw search rate difference (`nonwhite_search_rate − white_search_rate`). Exports a ranked CSV and produces scatter plots and box plots broken down by `HA_REGION` and `HA_SERVICE`.

**Packages:** `ggplot2`, `tidyr`, `stringr`, `dplyr`

### `modeling_search_probability.R`

Iterates over each officer in `data/regression_features/regression_features_select_officers.csv` and fits a logistic regression:

```
searched ~ sex + race + age_of_driver + texas_driver + age_of_car
```

Race is treated as a factor with White (`W`) as the reference level; Black (`B`) and Hispanic (`H`) are the comparison groups. Outputs model summaries and odds ratios with confidence intervals to `results/officer_models.txt`.

**Packages:** `stringr`

## Requirements

- R (≥ 3.x)
- Packages: `ggplot2`, `tidyr`, `stringr`, `dplyr`

Packages are auto-installed at the top of each script if missing.

## Data

The input data is not committed to this repository. The scripts expect a `data/` directory relative to the working directory (`~/personal_projects/texas-dps/`). The data originates from Texas DPS traffic stop records, grouped by officer ID and driver race.

## Data Source

Texas Department of Public Safety publishes traffic stop data under Texas state open records law. The Stanford Open Policing Project ([openpolicing.stanford.edu](https://openpolicing.stanford.edu)) also provides cleaned and standardized versions of state-level stop data.

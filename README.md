# Parent–Child Interactions as Mediators of Adolescent Outcomes
---
Lily X. Gates (Van Hout)  
University of Maryland  

*This project analyzes how parent–child interactions mediate the effects of negative life events (NLE) on adolescent internalizing and externalizing psychopathology outcomes.*

---

## Project Overview

This study uses longitudinal data collected from children and their mothers in Seattle, WA, to investigate the pathways through which early childhood negative life experiences impact adolescent mental health outcomes via parent–child interaction (PCI) behaviors.

Key features:
- **Predictors:** Negative Life Events (NLE) measured both by severity and number of events.
- **Mediators:** Parent–child interactions including warmth, rejection, structure, and interactiveness.
- **Outcomes:** Internalizing and externalizing psychopathology in adolescence.
- **Covariates:** Child sex, child age, and family socioeconomic status (income-to-needs ratio).

---

## Data

- `lab_data.csv`: Lab-collected outcome and PCI data.
- `NLE_data.csv`: Survey data on negative life events.
- `lab_data_var_labels.csv` and `NLE_template_spreadsheet.csv`: Variable name dictionaries.

---

## Analysis Pipeline

1. **Data Preparation and Cleaning**
    - Merging datasets by participant ID.
    - Renaming and converting variables for clarity and modeling.
    - Creating composite variables for NLE categories and PCIs.

2. **Descriptive Statistics**
    - Demographics and socioeconomic status summaries.
    - NLE descriptive statistics (number of events and severity).

3. **Parallel Mediation Models**
    - Using the PROCESS macro (model 4) to test mediation of NLE effects on internalizing and externalizing psychopathology by PCI variables.
    - Covariates included: child's sex, age, and socioeconomic status.
    - Models run separately for NLE severity and NLE number of events.

---

## Required R Packages

- `ggplot2`
- `jtools`
- `tidyverse`
- `readr`

---

## How to Run

1. Load required packages and source `process.R` for mediation modeling.
2. Run data cleaning and variable creation scripts.
3. Execute mediation models for internalizing and externalizing psychopathology outcomes.
4. Interpret output including direct, indirect, and total effects with bootstrapped confidence intervals.

---

## Notes

- Some missing data cases were removed before modeling (11 cases).
- Parent–child interaction variables are composites based on observed behaviors and affect coding.
- NLE variables are grouped theoretically but could be further analyzed using factor analysis.
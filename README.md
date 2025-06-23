# EU-GEI-study
R code for mediation analyses investigating facial affect recognition and negativity bias as pathways linking ethnicity and first-episode psychosis (EU-GEI study)

# Mediation Analysis Code for "The role of negativity bias and facial affect recognition in the increased odds of first-episode psychosis in ethnic minority populations: findings from the EU-GEI case-control study"

This repository contains the R code used for the analyses reported in our paper:

**[add citation link]**

## Contents
### Main analyses

- `1. multiple_imputation_missRanger_v3.R`: Script for data cleaning and preparation, multiple imputation, and descriptive statistics.
- `2. logistic_reg_dfar_neg.bias.R`: Script for logistic regression analysis with mediators and case-control outcome.
- `3.1 medflex_mediation_black_v3.R`: Main script for mediation analysis using the `medflex` package in Black group.
- `3.2 medflex_mediation_mixed_v3.R`: Main script for mediation analysis using the `medflex` package in Mixed group.
- `3.3 medflex_mediation_asian_v3.R`: Main script for mediation analysis using the `medflex` package in Asian group.
- `3.4 medflex_mediation_northafrican_v3.R`: Main script for mediation analysis using the `medflex` package in North African group.
- `3.5 medflex_mediation_other_v3.R`: Main script for mediation analysis using the `medflex` package in Other group.
- `4. dfar_negbias_scores_cases_controls.R`: Code for summary scores of DFAR and negativity bias in cases and controls per ethnic group.

### Sensitivity analyses 
Scripts for running alternative models (e.g., using processing speed as a substitute for parental social class as a proxy for cognitive ability).

## Requirements

- R (≥ 4.0.0)
- R packages: `medflex`, `lavaan`, `dplyr`, `ggplot2`, `boot`, etc.

Install required packages with:

```R
install.packages(c("medflex", "lavaan", "dplyr", "ggplot2", "boot"))

# EU-GEI-study
R code for mediation analyses investigating facial affect recognition and negativity bias as pathways linking ethnicity and first-episode psychosis (EU-GEI study)

# Mediation Analysis Code for "The role of negativity bias and facial affect recognition in the increased odds of first-episode psychosis in ethnic minority populations: findings from the EU-GEI case-control study"

This repository contains the R code used for the analyses reported in our paper:

**[add citation link]**

## Contents

- `data_preprocessing.R`: Scripts to clean and prepare the EU-GEI dataset.
- `mediation_models.R`: Main scripts for fitting natural effect models using the `medflex` package.
- `bootstrap_estimates.R`: Code to bootstrap confidence intervals for odds ratios and proportion mediated.
- `sensitivity_analyses.R`: Scripts for running alternative models (e.g., using processing speed as a proxy for cognitive ability).

## Requirements

- R (≥ 4.0.0)
- R packages: `medflex`, `lavaan`, `dplyr`, `ggplot2`, `boot`, etc.

Install required packages with:

```R
install.packages(c("medflex", "lavaan", "dplyr", "ggplot2", "boot"))

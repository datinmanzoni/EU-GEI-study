# Clear existing workspace 
rm(list=ls())

# Load packages 
library(lme4)
library(dplyr)
library(medflex)
library(parallel) 

# Set seed
set.seed(2022)

# Load imputed dataset
data_mixed <- read.csv("missRanger_postimputed_data_2025_sensitivity_analysis_IQ.csv")

table(data_mixed$subjectstatus)
str(data_mixed$subjectstatus)
table(data_mixed$gender)
str(data_mixed$gender)
table(data_mixed$country)
str(data_mixed$country)
table(data_mixed$ethnicity)
str(data_mixed$ethnicity)


# Convert variables into factors
data_mixed <- data_mixed %>%
  mutate(
    subjectstatus = factor(subjectstatus,
                           levels = c("control", "case")),
    gender = factor(gender,
                    levels = c("male", "female")),
    country = factor(country, 
                     levels = c("UK", "Holland", "Spain", "France", "Italy", "Brazil")),
    ethnicity = factor(ethnicity, 
                       levels = c('white', 'black', 'mixed', 'asian', 'north african', 'other'))
  )


# Filter the dataset for "white" and "mixed" subjects
data_mixed2 <- data_mixed %>%
  filter(ethnicity %in% c("white", "mixed"))

# Dummy coding 'group_mixed' variable: "white" = 0, "mixed" = 1
data_mixed2 <- data_mixed2 %>%
  mutate(group_mixed = ifelse(ethnicity == "mixed", 1, 0))

# Reorder columns
data_mixed2 <- data_mixed2 %>%
  relocate(group_mixed, .after = ethnicity)

# Checks
str(data_mixed2$group_mixed)
table(data_mixed2$group_mixed)

# Recode group_mixed into factor 
data_mixed2 <- data_mixed2 %>%
  mutate(group_mixed = factor(group_mixed, levels = c(0, 1)))

# Checks
str(data_mixed2$group_mixed)
table(data_mixed2$group_mixed)
levels(data_mixed2$group_mixed)

table(data_mixed2$subjectstatus, data_mixed2$group_mixed)
str(data_mixed2$subjectstatus)

table(data_mixed2$gender)
str(data_mixed2$gender)

table(data_mixed2$country)
str(data_mixed2$country)

str(data_mixed2$dfar.all)
str(data_mixed2$benton.total)
str(data_mixed2$symbol_coding_scaled)

# Save filtered mixed dataset
write.csv(data_mixed2,"data_mixed_filtered_sensitivity_analysis.csv")


######################################################################################################
######################################################################################################

# MEDIATION ANALYSIS 1: DFAR MEDIATOR 

## Crude model 

### Step 1: Fit an Outcome Regression Model (mediator model)
Mi0fit <-glm(
  formula = dfar.all ~ factor(group_mixed),
  data = data_mixed2,
  family= gaussian()
)

### Step 2: Create expanded dataset using inverse probability weighting
mi0data <- neWeight(Mi0fit)

### Step 3: Fit a natural effect model using weighted data (outcome model)

# Detect available cores and use parallel bootstrap
n_cores <- detectCores()

neMod0 <- neModel(
  formula = subjectstatus ~ group_mixed0 + group_mixed1,
  family = binomial(link = "logit"), 
  expData = mi0data, 
  se = "bootstrap", 
  nBoot = 1000,
  parallel = "multicore",
  ncpus = n_cores,
)

### Step 4: Decompose the effects into natural direct and indirect components
effdecomp0 <- neEffdecomp(neMod0)
effdecomp0_summary <- summary(effdecomp0)
effdecomp0_summary

### Step 5: Calculate Odds Ratios and CIs 
OR_crude_effdecomp <- exp(effdecomp0_summary$coefficients[, "Estimate"])
CI_OR_crude_effdecomp <- exp(confint(effdecomp0))

### Step 6: Calculate Proportion Mediated
OR_NIE <- OR_crude_effdecomp["natural indirect effect"]
OR_Total <- OR_crude_effdecomp["total effect"]
prop_mediated <- (OR_NIE - 1) / (OR_Total - 1) * 100

### Step 7: Print results
cat("\n--- Odds Ratios ---\n")
print(OR_crude_effdecomp)

cat("\n--- 95% Confidence Intervals (OR) ---\n")
print(CI_OR_crude_effdecomp)

cat("\n--- Proportion Mediated (OR) ---\n")
cat(round(prop_mediated, 2), "%\n")

######################################################################################################

## Model 1: age, gender, country

### Step 1: Fit an Outcome Regression Model (mediator model)
Mi1fit <-glm(
  formula = dfar.all ~ factor(group_mixed) + age + gender + country,
  data = data_mixed2,
  family= gaussian()
)

### Step 2: Create expanded dataset using inverse probability weighting
mi1data <- neWeight(Mi1fit)

### Step 3: Fit a natural effect model using weighted data (outcome model)

# Detect available cores and use parallel bootstrap
n_cores <- detectCores()

neMod1 <- neModel(
  formula = subjectstatus ~ group_mixed0 + group_mixed1 + age + gender + country,
  family = binomial(link = "logit"), 
  expData = mi1data, 
  se = "bootstrap", 
  nBoot = 1000,
  parallel = "multicore",
  ncpus = n_cores,
  progress = TRUE
)

### Step 4: Decompose the effects into natural direct and indirect components
effdecomp1 <- neEffdecomp(neMod1)
effdecomp1_summary <- summary(effdecomp1)
effdecomp1_summary

### Step 5: Calculate Odds Ratios and CIs 
OR_model1_effdecomp <- exp(effdecomp1_summary$coefficients[, "Estimate"])
CI_OR_model1_effdecomp <- exp(confint(effdecomp1))

### Step 6: Calculate Proportion Mediated
OR_NIE <- OR_model1_effdecomp["natural indirect effect"]
OR_Total <- OR_model1_effdecomp["total effect"]
prop_mediated <- (OR_NIE - 1) / (OR_Total - 1) * 100

### Step 7: Print results
cat("\n--- Odds Ratios ---\n")
print(OR_model1_effdecomp)

cat("\n--- 95% Confidence Intervals (OR) ---\n")
print(CI_OR_model1_effdecomp)

cat("\n--- Proportion Mediated (OR) ---\n")
cat(round(prop_mediated, 2), "%\n")

####################################################################################################

## Model 2: age, gender, country, processing speed, general facial recognition

### Step 1: Fit an Outcome Regression Model (mediator model)
Mi2fit <-glm(
  formula = dfar.all ~ factor(group_mixed) + age + gender + country + symbol_coding_scaled + benton.total,
  data = data_mixed2,
  family= gaussian()
)

### Step 2: Create expanded dataset using inverse probability weighting
mi2data <- neWeight(Mi2fit)

### Step 3: Fit a natural effect model using weighted data (outcome model)

# Detect available cores and use parallel bootstrap
n_cores <- detectCores()

neMod2 <- neModel(
  formula = subjectstatus ~ group_mixed0 + group_mixed1 + age + gender + country + symbol_coding_scaled + benton.total,
  family = binomial(link = "logit"), 
  expData = mi2data, 
  se = "bootstrap", 
  nBoot = 1000,
  parallel = "multicore",
  ncpus = n_cores,
  progress = TRUE
)

### Step 4: Decompose the effects into natural direct and indirect components
effdecomp2 <- neEffdecomp(neMod2)
effdecomp2_summary <- summary(effdecomp2)
effdecomp2_summary

### Step 5: Calculate Odds Ratios and CIs 
OR_model2_effdecomp <- exp(effdecomp2_summary$coefficients[, "Estimate"])
CI_OR_model2_effdecomp <- exp(confint(effdecomp2))

### Step 6: Calculate Proportion Mediated
OR_NIE <- OR_model2_effdecomp["natural indirect effect"]
OR_Total <- OR_model2_effdecomp["total effect"]
prop_mediated <- (OR_NIE - 1) / (OR_Total - 1) * 100

### Step 7: Print results
cat("\n--- Odds Ratios ---\n")
print(OR_model2_effdecomp)

cat("\n--- 95% Confidence Intervals (OR) ---\n")
print(CI_OR_model2_effdecomp)

cat("\n--- Proportion Mediated (OR) ---\n")
cat(round(prop_mediated, 2), "%\n")

####################################################################################################
####################################################################################################

# MEDIATION ANALYSIS 2: NEGATIVITY BIAS MEDIATOR 

## Crude model 

### Step 1: Fit an Outcome Regression Model (mediator model)
Mi0fit_2 <-glm(
  formula = negativity.bias ~ factor(group_mixed),
  data = data_mixed2,
  family= gaussian()
)

### Step 2: Create expanded dataset using inverse probability weighting
mi0data_2 <- neWeight(Mi0fit_2)

### Step 3: Fit a natural effect model using weighted data (outcome model)

# Detect available cores and use parallel bootstrap
n_cores <- detectCores()

neMod0_2 <- neModel(
  formula = subjectstatus ~ group_mixed0 + group_mixed1,
  family = binomial(link = "logit"), 
  expData = mi0data_2, 
  se = "bootstrap", 
  nBoot = 1000,
  parallel = "multicore",
  ncpus = n_cores,
  progress = TRUE
)

### Step 4: Decompose the effects into natural direct and indirect components
effdecomp0_2 <- neEffdecomp(neMod0_2)
effdecomp0_summary_2 <- summary(effdecomp0_2)
effdecomp0_summary_2

### Step 5: Calculate Odds Ratios and CIs 
OR_crude_effdecomp_2 <- exp(effdecomp0_summary_2$coefficients[, "Estimate"])
CI_OR_crude_effdecomp_2 <- exp(confint(effdecomp0_2))

### Step 6: Calculate Proportion Mediated
OR_NIE <- OR_crude_effdecomp_2["natural indirect effect"]
OR_Total <- OR_crude_effdecomp_2["total effect"]
prop_mediated <- (OR_NIE - 1) / (OR_Total - 1) * 100

### Step 7: Print results
cat("\n--- Odds Ratios ---\n")
print(OR_crude_effdecomp_2)

cat("\n--- 95% Confidence Intervals (OR) ---\n")
print(CI_OR_crude_effdecomp_2)

cat("\n--- Proportion Mediated (OR) ---\n")
cat(round(prop_mediated, 2), "%\n")

######################################################################################################

## Model 1: age, gender, country

### Step 1: Fit an Outcome Regression Model (mediator model)
Mi1fit_2 <-glm(
  formula = negativity.bias ~ factor(group_mixed) + age + gender + country,
  data = data_mixed2,
  family= gaussian()
)

### Step 2: Create expanded dataset using inverse probability weighting
mi1data_2 <- neWeight(Mi1fit_2)

### Step 3: Fit a natural effect model using weighted data (outcome model)

# Detect available cores and use parallel bootstrap
n_cores <- detectCores()

neMod1_2 <- neModel(
  formula = subjectstatus ~ group_mixed0 + group_mixed1 + age + gender + country,
  family = binomial(link = "logit"), 
  expData = mi1data_2, 
  se = "bootstrap", 
  nBoot = 1000,
  parallel = "multicore",
  ncpus = n_cores,
  progress = TRUE
)

### Step 4: Decompose the effects into natural direct and indirect components
effdecomp1_2 <- neEffdecomp(neMod1_2)
effdecomp1_summary_2 <- summary(effdecomp1_2)
effdecomp1_summary_2

### Step 5: Calculate Odds Ratios and CIs 
OR_model1_effdecomp_2 <- exp(effdecomp1_summary_2$coefficients[, "Estimate"])
CI_OR_model1_effdecomp_2 <- exp(confint(effdecomp1_2))

### Step 6: Calculate Proportion Mediated
OR_NIE <- OR_model1_effdecomp_2["natural indirect effect"]
OR_Total <- OR_model1_effdecomp_2["total effect"]
prop_mediated <- (OR_NIE - 1) / (OR_Total - 1) * 100

### Step 7: Print results
cat("\n--- Odds Ratios ---\n")
print(OR_model1_effdecomp_2)

cat("\n--- 95% Confidence Intervals (OR) ---\n")
print(CI_OR_model1_effdecomp_2)

cat("\n--- Proportion Mediated (OR) ---\n")
cat(round(prop_mediated, 2), "%\n")

##########################################################################################################

## Model 2: age, gender, country, processing speed, general facial recognition

### Step 1: Fit an Outcome Regression Model (mediator model)
Mi2fit_2 <-glm(
  formula = negativity.bias ~ factor(group_mixed) + age + gender + country + symbol_coding_scaled + benton.total,
  data = data_mixed2,
  family= gaussian()
)

### Step 2: Create expanded dataset using inverse probability weighting
mi2data_2 <- neWeight(Mi2fit_2)

### Step 3: Fit a natural effect model using weighted data (outcome model)

# Detect available cores and use parallel bootstrap
n_cores <- detectCores()

neMod2_2 <- neModel(
  formula = subjectstatus ~ group_mixed0 + group_mixed1 + age + gender + country + symbol_coding_scaled + benton.total,
  family = binomial(link = "logit"), 
  expData = mi2data_2, 
  se = "bootstrap", 
  nBoot = 1000,
  parallel = "multicore",
  ncpus = n_cores,
  progress = TRUE
)

### Step 4: Decompose the effects into natural direct and indirect components
effdecomp2_2 <- neEffdecomp(neMod2_2)
effdecomp2_summary_2 <- summary(effdecomp2_2)
effdecomp2_summary_2

### Step 5: Calculate Odds Ratios and CIs 
OR_model2_effdecomp_2 <- exp(effdecomp2_summary_2$coefficients[, "Estimate"])
CI_OR_model2_effdecomp_2 <- exp(confint(effdecomp2_2))

### Step 6: Calculate Proportion Mediated
OR_NIE <- OR_model2_effdecomp_2["natural indirect effect"]
OR_Total <- OR_model2_effdecomp_2["total effect"]
prop_mediated <- (OR_NIE - 1) / (OR_Total - 1) * 100

### Step 7: Print results
cat("\n--- Odds Ratios ---\n")
print(OR_model2_effdecomp_2)

cat("\n--- 95% Confidence Intervals (OR) ---\n")
print(CI_OR_model2_effdecomp_2)

cat("\n--- Proportion Mediated (OR) ---\n")
cat(round(prop_mediated, 2), "%\n")

####################################################################################
####################################################################################

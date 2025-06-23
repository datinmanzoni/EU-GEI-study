# Clear existing workspace 
rm(list=ls())

# Load packages 
library(lme4)
library(lmerTest)
library(dplyr)

# Set seed 
set.seed(2022)

# Load imputed dataset
myDf <- read.csv("missRanger_postimputed_data_2025_v3.csv")

# Checks 
summary(myDf$dfar.all)
summary(myDf$negativity.bias)

# Recode variables 
myDf <- myDf %>%
  mutate(
    subjectstatus = factor(subjectstatus,
                           levels = c("control", "case")),
    gender = factor(gender,
                    levels = c("male", "female")),
    country = factor(country, 
                     levels = c("UK", "Holland", "Spain", "France", "Italy", "Brazil")),
    parentssocialclass = factor(parentssocialclass, 
                                levels = c("salariat", "intermediate", "working class", "longterm unemployed")),
    ethnicity = factor(ethnicity, 
                       levels = c('white', 'black', 'mixed', 'asian', 'north african', 'other'))
  )

# Checks
str(myDf$subjectstatus)
table(myDf$subjectstatus)

str(myDf$gender)
table(myDf$gender)

str(myDf$country)
table(myDf$country)

str(myDf$parentssocialclass)
table(myDf$parentssocialclass)

str(myDf$ethnicity)
table(myDf$ethnicity)

# Combine lower levels of parentssocialclass into 'lower class' category
myDf <- myDf %>%
  mutate(parentssocialclass2 = recode_factor(parentssocialclass,
                                             "longterm unemployed" = "lower class",
                                             "working class" = "lower class"))

# Remove unused levels from the factor
myDf$parentssocialclass2 <- droplevels(myDf$parentssocialclass2)

# Check updated table
table(myDf$parentssocialclass2)

#################################################################################
# Logistic regression models 
#################################################################################

# WHITE GROUP 

# Filter the dataset for "white" subjects
myDf1 <- myDf %>%
  filter(ethnicity %in% c("white"))

# Create quadratic term for age due to non-linearity with subjectstatus (assumption violation)
myDf1$age_sq <- myDf1$age^2

# 1a) DFAR total adjusted for age, gender and BFRT
m1_a <- glm(subjectstatus ~ dfar.all + age_sq + gender + benton.total, 
            data = myDf1, family = binomial(link = "logit"))

summary(m1_a)

### Calculate Odds Ratios for the Effects
OR_m1_a <- exp((summary(m1_a))$coefficients[, "Estimate"])
print(OR_m1_a)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m1_a))

# 1b) DFAR total adjusted for age, gender, BFRT and parent social class  
m1_b <- glm(subjectstatus ~ dfar.all + age_sq + gender + benton.total + parentssocialclass2, 
            data = myDf1, family = binomial(link = "logit"))

summary(m1_b)

### Calculate Odds Ratios for the Effects
OR_m1_b <- exp((summary(m1_b))$coefficients[, "Estimate"])
print(OR_m1_b)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m1_b))

# 1c) Negativity bias adjusted for age, gender, BFRT
m1_c <- glm(subjectstatus ~ negativity.bias + age_sq + gender + benton.total,
             data = myDf1, family = binomial(link = "logit"))

summary(m1_c)

### Calculate Odds Ratios for the Effects
OR_m1_c <- exp((summary(m1_c))$coefficients[, "Estimate"])
print(OR_m1_c)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m1_c))

# 1d) Negativity bias adjusted for age, gender, BFRT and parent social class  
m1_d <- glm(subjectstatus ~ negativity.bias + age_sq + gender + benton.total + parentssocialclass2, 
             data = myDf1, family = binomial(link = "logit"))

summary(m1_d)

### Calculate Odds Ratios for the Effects
OR_m1_d <- exp((summary(m1_d))$coefficients[, "Estimate"])
print(OR_m1_d)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m1_d))

##################################################################################

# BLACK GROUP 


# Filter the dataset for "black" subjects
myDf2 <- myDf %>%
  filter(ethnicity %in% c("black"))

# Create quadratic term for age and BFRT due to non-linearity with subjectstatus (assumption violation)
myDf2$age_sq <- myDf2$age^2
myDf2$benton.total_sq <- myDf2$benton.total^2

# 2a) DFAR total adjusted for age, gender and BFRT
m2_a <- glm(subjectstatus ~ dfar.all + age_sq + gender + benton.total_sq, 
             data = myDf2, family = binomial(link = "logit"))

summary(m2_a)

### Calculate Odds Ratios for the Effects
OR_m2_a <- exp((summary(m2_a))$coefficients[, "Estimate"])
print(OR_m2_a)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m2_a))

# 2b) DFAR total adjusted for age, gender, BFRT and parent social class  
m2_b <- glm(subjectstatus ~ dfar.all + age_sq + gender + benton.total_sq + parentssocialclass2, 
             data = myDf2, family = binomial(link = "logit"))

summary(m2_b)

### Calculate Odds Ratios for the Effects
OR_m2_b <- exp((summary(m2_b))$coefficients[, "Estimate"])
print(OR_m2_b)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m2_b))

# 2c) Negativity bias adjusted for age, gender, BFRT
m2_c <- glm(subjectstatus ~ negativity.bias + age_sq + gender + benton.total_sq, 
             data = myDf2, family = binomial(link = "logit"))

summary(m2_c)

### Calculate Odds Ratios for the Effects
OR_m2_c <- exp((summary(m2_c))$coefficients[, "Estimate"])
print(OR_m2_c)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m2_c))


# 2d) Negativity bias adjusted for age, gender, BFRT and parent social class  
m2_d <- glm(subjectstatus ~ negativity.bias + age_sq + gender + benton.total_sq + parentssocialclass2, 
             data = myDf2, family = binomial(link = "logit"))

summary(m2_d)

### Calculate Odds Ratios for the Effects
OR_m1_d <- exp((summary(m1_d))$coefficients[, "Estimate"])
print(OR_m1_d)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m1_d))

##################################################################################

# MIXED GROUP 

# Filter the dataset for "mixed" subjects
myDf3 <- myDf %>%
  filter(ethnicity %in% c("mixed"))

# 3a) DFAR total adjusted for age, gender and BFRT
m3_a <- glm(subjectstatus ~ dfar.all + age + gender + benton.total, 
             data = myDf3, family = binomial(link = "logit"))

summary(m3_a)

### Calculate Odds Ratios for the Effects
OR_m3_a <- exp((summary(m3_a))$coefficients[, "Estimate"])
print(OR_m3_a)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m3_a))

# 3b) DFAR total adjusted for age, gender, BFRT and parent social class  
m3_b <- glm(subjectstatus ~ dfar.all + age + gender + benton.total + parentssocialclass2, 
             data = myDf3, family = binomial(link = "logit"))

summary(m3_b)

### Calculate Odds Ratios for the Effects
OR_m3_b <- exp((summary(m3_b))$coefficients[, "Estimate"])
print(OR_m3_b)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m3_b))

# 3c) Negativity bias adjusted for age, gender, BFRT
m3_c <- glm(subjectstatus ~ negativity.bias + age + gender + benton.total, 
             data = myDf3, family = binomial(link = "logit"))

summary(m3_c)

### Calculate Odds Ratios for the Effects
OR_m3_c <- exp((summary(m3_c))$coefficients[, "Estimate"])
print(OR_m3_c)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m3_c))

# 3d) Negativity bias adjusted for age, gender, BFRT and parent social class  
m3_d <- glm(subjectstatus ~ negativity.bias + age + gender + benton.total + parentssocialclass2, 
             data = myDf3, family = binomial(link = "logit"))

summary(m3_d)

### Calculate Odds Ratios for the Effects
OR_m3_d <- exp((summary(m3_d))$coefficients[, "Estimate"])
print(OR_m3_d)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m3_d))

##################################################################################

# ASIAN GROUP 

# Filter the dataset for "asian" subjects
myDf4 <- myDf %>%
  filter(ethnicity %in% c("asian"))


# 4a) DFAR total adjusted for age, gender and BFRT
m4_a <- glm(subjectstatus ~ dfar.all + age + gender + benton.total, 
             data = myDf4, family = binomial(link = "logit"))

summary(m4_a)

### Calculate Odds Ratios for the Effects
OR_m4_a <- exp((summary(m4_a))$coefficients[, "Estimate"])
print(OR_m4_a)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m4_a))

# 4b) DFAR total adjusted for age, gender, BFRT and parent social class  
m4_b <- glm(subjectstatus ~ dfar.all + age + gender + benton.total + parentssocialclass2, 
             data = myDf4, family = binomial(link = "logit"))

summary(m4_b)

### Calculate Odds Ratios for the Effects
OR_m4_b <- exp((summary(m4_b))$coefficients[, "Estimate"])
print(OR_m4_b)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m4_b))

# 4c) Negativity bias adjusted for age, gender, BFRT
m4_c <- glm(subjectstatus ~ negativity.bias + age + gender + benton.total, 
             data = myDf4, family = binomial(link = "logit"))

summary(m4_c)

### Calculate Odds Ratios for the Effects
OR_m4_c <- exp((summary(m4_c))$coefficients[, "Estimate"])
print(OR_m4_c)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m4_c))

# 4d) Negativity bias adjusted for age, gender, BFRT and parent social class  
m4_d <- glm(subjectstatus ~ negativity.bias + age + gender + benton.total + parentssocialclass2, 
             data = myDf4, family = binomial(link = "logit"))

summary(m4_d)

### Calculate Odds Ratios for the Effects
OR_m4_d <- exp((summary(m4_d))$coefficients[, "Estimate"])
print(OR_m4_d)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m4_d))

##################################################################################

# NORTH AFRICAN GROUP 

# Filter the dataset for "north african" subjects
myDf5 <- myDf %>%
  filter(ethnicity %in% c("north african"))

# 5a) DFAR total adjusted for age, gender and BFRT
m5_a <- glm(subjectstatus ~ dfar.all + age + gender + benton.total, 
             data = myDf5, family = binomial(link = "logit"))

summary(m5_a)

### Calculate Odds Ratios for the Effects
OR_m5_a <- exp((summary(m5_a))$coefficients[, "Estimate"])
print(OR_m5_a)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m5_a))

# 5b) DFAR total adjusted for age, gender, BFRT and parent social class  
m5_b <- glm(subjectstatus ~ dfar.all + age + gender + benton.total + parentssocialclass2, 
             data = myDf5, family = binomial(link = "logit"))

summary(m5_b)

### Calculate Odds Ratios for the Effects
OR_m5_b <- exp((summary(m5_b))$coefficients[, "Estimate"])
print(OR_m5_b)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m5_b))

# 5c) Negativity bias adjusted for age, gender, BFRT
m5_c <- glm(subjectstatus ~ negativity.bias + age + gender + benton.total, 
             data = myDf5, family = binomial(link = "logit"))

summary(m5_c)

### Calculate Odds Ratios for the Effects
OR_m5_c <- exp((summary(m5_c))$coefficients[, "Estimate"])
print(OR_m5_c)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m5_c))

# 5d) Negativity bias adjusted for age, gender, BFRT and parent social class  
m5_d <- glm(subjectstatus ~ negativity.bias + age + gender + benton.total + parentssocialclass2, 
             data = myDf5, family = binomial(link = "logit"))

summary(m5_d)

### Calculate Odds Ratios for the Effects
OR_m5_d <- exp((summary(m5_d))$coefficients[, "Estimate"])
print(OR_m5_d)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m5_d))

##################################################################################

# OTHER GROUP 

# Filter the dataset for "other" subjects
myDf6 <- myDf %>%
  filter(ethnicity %in% c("other"))

# 6a) DFAR total adjusted for age, gender and BFRT
m6_a <- glm(subjectstatus ~ dfar.all + age + gender + benton.total, 
             data = myDf6, family = binomial(link = "logit"))

summary(m6_a)

### Calculate Odds Ratios for the Effects
OR_m6_a <- exp((summary(m6_a))$coefficients[, "Estimate"])
print(OR_m6_a)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m6_a))

# 6b) DFAR total adjusted for age, gender, BFRT and parent social class  
m6_b <- glm(subjectstatus ~ dfar.all + age + gender + benton.total + parentssocialclass2, 
             data = myDf6, family = binomial(link = "logit"))

summary(m6_b)

### Calculate Odds Ratios for the Effects
OR_m6_b <- exp((summary(m6_b))$coefficients[, "Estimate"])
print(OR_m6_b)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m6_b))


# 6c) Negativity bias adjusted for age, gender, BFRT
m6_c <- glm(subjectstatus ~ negativity.bias + age + gender + benton.total, 
             data = myDf6, family = binomial(link = "logit"))

summary(m6_c)

### Calculate Odds Ratios for the Effects
OR_m6_c <- exp((summary(m6_c))$coefficients[, "Estimate"])
print(OR_m6_c)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m6_c))

# 6d) Negativity bias adjusted for age, gender, BFRT and parent social class  
m6_d <- glm(subjectstatus ~ negativity.bias + age + gender + benton.total + parentssocialclass2, 
             data = myDf6, family = binomial(link = "logit"))

summary(m6_d)

### Calculate Odds Ratios for the Effects
OR_m6_d <- exp((summary(m6_d))$coefficients[, "Estimate"])
print(OR_m6_d)

### Calculate Confidence Intervals for Odds Ratios
exp(confint(m6_d))



###################################################################################
###################################################################################

# Logistic regression assumption checks

## 1) Binary outcome/dependent variable 

## 2) Observations should be independent (i.e., no repeated measurements)

## 3) No multicollinearity between predictors (VIF > 5-10 consider removing or combining variables)

## 4) Linearity of independent variables and log odds of the dependent variable (BoxTidwell test)

library(car)

# WHITE GROUP 

##### 3) No multicollinearity between predictors
car::vif(m1_a)
car::vif(m1_b)
car::vif(m1_c)
car::vif(m1_d)

#### 4) Linearity of independent variables and log odds of continuous dependent variable

# Create interaction terms with log-transformed values
myDf1$log_dfar.all <- myDf1$dfar.all * log(myDf1$dfar.all)
myDf1$log_age <- myDf1$age_sq * log(myDf1$age_sq)
myDf1$log_benton.total <- myDf1$benton.total * log(myDf1$benton.total)
myDf1$log_negativity.bias <- myDf1$negativity.bias * log(myDf1$negativity.bias)

# Fit the model including the interaction terms
m1_a_glm <- glm(subjectstatus ~ dfar.all + log_dfar.all + 
               age_sq + log_age + 
               benton.total + log_benton.total + 
               gender,  # Gender stays as a covariate but NOT in Box-Tidwell terms
             data = myDf1, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m1_b_glm <- glm(subjectstatus ~ dfar.all + log_dfar.all + 
                  age_sq + log_age + 
                  benton.total + log_benton.total + 
                  gender + parentssocialclass2,  # Gender and parents social status stay as covariates but NOT in Box-Tidwell terms
                data = myDf1, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m1_c_glm <- glm(subjectstatus ~ negativity.bias + log_negativity.bias + 
                  age_sq + log_age + 
                  benton.total + log_benton.total + 
                  gender,  # Gender stays as a covariate but NOT in Box-Tidwell terms
                data = myDf1, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m1_d_glm <- glm(subjectstatus ~ negativity.bias + log_negativity.bias + 
                  age_sq + log_age + 
                  benton.total + log_benton.total + 
                  gender + parentssocialclass2,  # Gender and parentssocialstatus stay as a covariates but NOT in Box-Tidwell terms
                data = myDf1, family = binomial(link = "logit"))

summary(m1_a_glm)  # If log-transformed terms are significant, linearity assumption is violated
summary(m1_b_glm)
summary(m1_c_glm)
summary(m1_d_glm)

################################################################################

# BLACK GROUP 

##### 3) No multicollinearity between predictors
car::vif(m2_a)
car::vif(m2_b)
car::vif(m2_c)
car::vif(m2_d)

#### 4) Linearity of independent variables and log odds of continuous dependent variable

# Create interaction terms with log-transformed values
myDf2$log_dfar.all <- myDf2$dfar.all * log(myDf2$dfar.all)
myDf2$log_age <- myDf2$age_sq * log(myDf2$age_sq)
myDf2$log_benton.total <- myDf2$benton.total_sq * log(myDf2$benton.total_sq)
myDf2$log_negativity.bias <- myDf2$negativity.bias * log(myDf2$negativity.bias)

# Fit the model including the interaction terms
m2_a_glm <- glm(subjectstatus ~ dfar.all + log_dfar.all + 
                  age_sq + log_age + 
                  benton.total_sq + log_benton.total + 
                  gender,  # Gender stays as a covariate but NOT in Box-Tidwell terms
                data = myDf2, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m2_b_glm <- glm(subjectstatus ~ dfar.all + log_dfar.all + 
                  age_sq + log_age + 
                  benton.total_sq + log_benton.total + 
                  gender + parentssocialclass2,  # Gender and parents social status stay as covariates but NOT in Box-Tidwell terms
                data = myDf2, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m2_c_glm <- glm(subjectstatus ~ negativity.bias + log_negativity.bias + 
                  age_sq + log_age + 
                  benton.total_sq + log_benton.total + 
                  gender,  # Gender stays as a covariate but NOT in Box-Tidwell terms
                data = myDf2, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m2_d_glm <- glm(subjectstatus ~ negativity.bias + log_negativity.bias + 
                  age_sq + log_age + 
                  benton.total_sq + log_benton.total + 
                  gender + parentssocialclass2,  # Gender and parentssocialstatus stay as a covariates but NOT in Box-Tidwell terms
                data = myDf2, family = binomial(link = "logit"))

summary(m2_a_glm)  # If log-transformed terms are significant, linearity assumption is violated
summary(m2_b_glm)
summary(m2_c_glm)
summary(m2_d_glm)

################################################################################

# MIXED GROUP 

##### 3) No multicollinearity between predictors
car::vif(m3_a)
car::vif(m3_b)
car::vif(m3_c)
car::vif(m3_d)

#### 4) Linearity of independent variables and log odds of continuous dependent variable

# Create interaction terms with log-transformed values
myDf3$log_dfar.all <- myDf3$dfar.all * log(myDf3$dfar.all)
myDf3$log_age <- myDf3$age * log(myDf3$age)
myDf3$log_benton.total <- myDf3$benton.total * log(myDf3$benton.total)
myDf3$log_negativity.bias <- myDf3$negativity.bias * log(myDf3$negativity.bias)

# Fit the model including the interaction terms
m3_a_glm <- glm(subjectstatus ~ dfar.all + log_dfar.all + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender,  # Gender stays as a covariate but NOT in Box-Tidwell terms
                data = myDf3, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m3_b_glm <- glm(subjectstatus ~ dfar.all + log_dfar.all + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender + parentssocialclass2,  # Gender and parents social status stay as covariates but NOT in Box-Tidwell terms
                data = myDf3, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m3_c_glm <- glm(subjectstatus ~ negativity.bias + log_negativity.bias + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender,  # Gender stays as a covariate but NOT in Box-Tidwell terms
                data = myDf3, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m3_d_glm <- glm(subjectstatus ~ negativity.bias + log_negativity.bias + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender + parentssocialclass2,  # Gender and parentssocialstatus stay as a covariates but NOT in Box-Tidwell terms
                data = myDf3, family = binomial(link = "logit"))

summary(m3_a_glm)  # If log-transformed terms are significant, linearity assumption is violated
summary(m3_b_glm)
summary(m3_c_glm)
summary(m3_d_glm)

################################################################################

# ASIAN GROUP 

##### 3) No multicollinearity between predictors
car::vif(m4_a)
car::vif(m4_b)
car::vif(m4_c)
car::vif(m4_d)

#### 4) Linearity of independent variables and log odds of continuous dependent variable

# Create interaction terms with log-transformed values
myDf4$log_dfar.all <- myDf4$dfar.all * log(myDf4$dfar.all)
myDf4$log_age <- myDf4$age * log(myDf4$age)
myDf4$log_benton.total <- myDf4$benton.total * log(myDf4$benton.total)
myDf4$log_negativity.bias <- myDf4$negativity.bias * log(myDf4$negativity.bias)

# Fit the model including the interaction terms
m4_a_glm <- glm(subjectstatus ~ dfar.all + log_dfar.all + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender,  # Gender stays as a covariate but NOT in Box-Tidwell terms
                data = myDf4, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m4_b_glm <- glm(subjectstatus ~ dfar.all + log_dfar.all + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender + parentssocialclass2,  # Gender and parents social status stay as covariates but NOT in Box-Tidwell terms
                data = myDf4, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m4_c_glm <- glm(subjectstatus ~ negativity.bias + log_negativity.bias + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender,  # Gender stays as a covariate but NOT in Box-Tidwell terms
                data = myDf4, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m4_d_glm <- glm(subjectstatus ~ negativity.bias + log_negativity.bias + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender + parentssocialclass2,  # Gender and parentssocialstatus stay as a covariates but NOT in Box-Tidwell terms
                data = myDf4, family = binomial(link = "logit"))

summary(m4_a_glm)  # If log-transformed terms are significant, linearity assumption is violated
summary(m4_b_glm)
summary(m4_c_glm)
summary(m4_d_glm)

################################################################################

# NORTH AFRICAN GROUP 

##### 3) No multicollinearity between predictors
car::vif(m5_a)
car::vif(m5_b)
car::vif(m5_c)
car::vif(m5_d)

#### 4) Linearity of independent variables and log odds of continuous dependent variable

# Create interaction terms with log-transformed values
myDf5$log_dfar.all <- myDf5$dfar.all * log(myDf5$dfar.all)
myDf5$log_age <- myDf5$age * log(myDf5$age)
myDf5$log_benton.total <- myDf5$benton.total * log(myDf5$benton.total)
myDf5$log_negativity.bias <- myDf5$negativity.bias * log(myDf5$negativity.bias)

# Fit the model including the interaction terms
m5_a_glm <- glm(subjectstatus ~ dfar.all + log_dfar.all + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender,  # Gender stays as a covariate but NOT in Box-Tidwell terms
                data = myDf5, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m5_b_glm <- glm(subjectstatus ~ dfar.all + log_dfar.all + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender + parentssocialclass2,  # Gender and parents social status stay as covariates but NOT in Box-Tidwell terms
                data = myDf5, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m5_c_glm <- glm(subjectstatus ~ negativity.bias + log_negativity.bias + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender,  # Gender stays as a covariate but NOT in Box-Tidwell terms
                data = myDf5, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m5_d_glm <- glm(subjectstatus ~ negativity.bias + log_negativity.bias + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender + parentssocialclass2,  # Gender and parentssocialstatus stay as a covariates but NOT in Box-Tidwell terms
                data = myDf5, family = binomial(link = "logit"))

summary(m5_a_glm)  # If log-transformed terms are significant, linearity assumption is violated
summary(m5_b_glm)
summary(m5_c_glm)
summary(m5_d_glm)

################################################################################

# OTHER GROUP 

##### 3) No multicollinearity between predictors
car::vif(m6_a)
car::vif(m6_b)
car::vif(m6_c)
car::vif(m6_d)

#### 4) Linearity of independent variables and log odds of continuous dependent variable

# Create interaction terms with log-transformed values
myDf6$log_dfar.all <- myDf6$dfar.all * log(myDf6$dfar.all)
myDf6$log_age <- myDf6$age * log(myDf6$age)
myDf6$log_benton.total <- myDf6$benton.total * log(myDf6$benton.total)
myDf6$log_negativity.bias <- myDf6$negativity.bias * log(myDf6$negativity.bias)

# Fit the model including the interaction terms
m6_a_glm <- glm(subjectstatus ~ dfar.all + log_dfar.all + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender,  # Gender stays as a covariate but NOT in Box-Tidwell terms
                data = myDf6, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m6_b_glm <- glm(subjectstatus ~ dfar.all + log_dfar.all + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender + parentssocialclass2,  # Gender and parents social status stay as covariates but NOT in Box-Tidwell terms
                data = myDf6, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m6_c_glm <- glm(subjectstatus ~ negativity.bias + log_negativity.bias + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender,  # Gender stays as a covariate but NOT in Box-Tidwell terms
                data = myDf6, family = binomial(link = "logit"))

# Fit the model including the interaction terms
m6_d_glm <- glm(subjectstatus ~ negativity.bias + log_negativity.bias + 
                  age + log_age + 
                  benton.total + log_benton.total + 
                  gender + parentssocialclass2,  # Gender and parentssocialstatus stay as a covariates but NOT in Box-Tidwell terms
                data = myDf6, family = binomial(link = "logit"))

summary(m6_a_glm)  # If log-transformed terms are significant, linearity assumption is violated
summary(m6_b_glm)
summary(m6_c_glm)
summary(m6_d_glm)


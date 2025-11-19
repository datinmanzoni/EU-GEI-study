# Clear existing workspace 
rm(list=ls())

# Load packages 
library(haven)
library(missRanger)
library(lme4)
library(dplyr)

# Set seed
set.seed(2022)

# Read pre-imputed raw data
data_raw <- read_sav("2023_finaldata_preimputation.sav")  
data_preimp <- as.data.frame(data_raw)

# Checks
colnames(data_preimp)

# Rename variables
data_preimp$subjectID <- data_preimp$st_subjid
data_preimp$gender <- data_preimp$mrc1_socde01
data_preimp$age <- data_preimp$mrc1_socde02
data_preimp$subjectstatus <- data_preimp$mrc1_socde01a_new
data_preimp$ethnicity <- data_preimp$mrc1_socde03
data_preimp$ethnicitysitespecific <- data_preimp$mrc1_socde04_amnd
data_preimp$ageofmigration <- data_preimp$mrc1_socde07
data_preimp$site <- data_preimp$site
data_preimp$country <- data_preimp$country
data_preimp$age_firstcontact <- data_preimp$age_fc_cga_new
data_preimp$relationship <- data_preimp$mrc2_socde30_xx
data_preimp$educationlevel <- data_preimp$mrc2_socde37_xx
data_preimp$parental_psychosis_chk <- data_preimp$parental_psychosis_chk
data_preimp$parentssocialclass <- data_preimp$mrc1_socde30_4grp
data_preimp$edulvl <- data_preimp$edulvl
data_preimp$socialclass <- data_preimp$mrc1_socde22_4grp
data_preimp$ctq_tot <- data_preimp$ctq_tot
data_preimp$cannabisuse <- data_preimp$ceq15_1
data_preimp$IQ <- data_preimp$totschatiq_x
data_preimp$IQ.brazil<- data_preimp$stimatedqi
data_preimp$benton.total <- data_preimp$bfrtot
data_preimp$benton.recoded <- data_preimp$bfrtot_scale
data_preimp$migrationstatus <- data_preimp$MigrantStatus
data_preimp$living <- data_preimp$mrc2_socde01_x
data_preimp$languagedistance <- data_preimp$lang_dist
data_preimp$misinterpretation.neutral <- data_preimp$misinterpretation_neutral
data_preimp$misinterpretation.happy <- data_preimp$misinterpretation_happy
data_preimp$negativity.bias <- data_preimp$negativitybias
data_preimp$paternalage <- data_preimp$pat_age
data_preimp$maternalage <- data_preimp$pat_age
data_preimp$dfar.complete <- data_preimp$dfar_comp
data_preimp$dfar.all <- data_preimp$dfar_all
data_preimp$dfar.neutral <- data_preimp$dfarneutr
data_preimp$dfar.happy <- data_preimp$dfarhappy
data_preimp$dfar.fear <- data_preimp$dfarfright
data_preimp$dfar.angry <- data_preimp$dfarangry
data_preimp$Hu.neutral <- data_preimp$Hu_neutr
data_preimp$Hu.happy <- data_preimp$Hu_happy
data_preimp$Hu.fear <- data_preimp$Hu_fright
data_preimp$Hu.angry <- data_preimp$Hu_angry
data_preimp$p.neutralhappy <- data_preimp$pneutralhappy
data_preimp$p.neutralfear <- data_preimp$pneutralfear
data_preimp$p.neutralanger <- data_preimp$pneutralanger
data_preimp$p.happyfear <- data_preimp$phappyfear
data_preimp$p.happyangry <- data_preimp$phappyanger
data_preimp$p.happyneutral <- data_preimp$phappyneutral
data_preimp$p.fearanger <- data_preimp$pfearanger
data_preimp$p.fearhappy <- data_preimp$pfearhappy
data_preimp$p.fearneutral <- data_preimp$pfearneutral
data_preimp$p.angerneutral <- data_preimp$pangerneutral
data_preimp$p.angerhappy <- data_preimp$pangerhappy
data_preimp$p.angerfear <- data_preimp$pangerfear
data_preimp$IQ <- data_preimp$totschatiq_x
data_preimp$IQ.brazil <- data_preimp$stimatedqi
data_preimp$symbol_coding_scaled_EU <- data_preimp$symbgs_x
data_preimp$symbol_coding_scaled_BR <- data_preimp$digitscaled

# Check for missings in IQ
table(is.na(data_preimp$IQ))
table(is.na(data_preimp$IQ.brazil))
table(is.na(data_preimp$symbol_coding_scaled_EU))
table(is.na(data_preimp$symbol_coding_scaled_BR))

# Merge EU and Brazil IQ variables 
data_preimp$IQ_total <- coalesce(data_preimp$IQ, data_preimp$IQ.brazil)
data_preimp$symbol_coding_scaled <- coalesce(data_preimp$symbol_coding_scaled_EU, data_preimp$symbol_coding_scaled_BR)


# Check execution
sum(!is.na(data_preimp$symbol_coding_scaled))
sum(!is.na(data_preimp$IQ_total))


# Recode variables as factors
data_preimp$gender <- factor(data_preimp$gender, levels = c("1","2"), labels = c("male", "female"))
data_preimp$ethnicity <- factor(data_preimp$ethnicity, levels = c("1","2","3","4","5","6"), labels = c('white', 'black', 'mixed', 'asian', 'north african', 'other'))
data_preimp$subjectstatus <- factor(data_preimp$subjectstatus, levels = c("0","1"), labels = c("control", "case"))
data_preimp$country <- factor(data_preimp$country, levels = c("1", "2", "3", "4", "5", "6"), labels = c("UK", "Holland", "Spain", "France", "Italy", "Brazil"))
data_preimp$parentssocialclass <- factor(data_preimp$parentssocialclass, levels = c("0", "1", "2", "3"), labels = c("salariat", "intermediate", "working class", "longterm unemployed"))


# Checks 
table(data_preimp$subjectstatus)
table(data_preimp$gender)
table(data_preimp$ethnicity) 
table(data_preimp$country)
table(data_preimp$subjectstatus, data_preimp$parentssocialclass)

# Create filtered dataframe 
data_preimp_filtered2 <- subset(data_preimp, select = c(subjectID, subjectstatus, ethnicity, age, gender, ethnicitysitespecific, 
                                                       ageofmigration, site, country, migrationstatus, age_firstcontact, educationlevel, 
                                                       edulvl, living, relationship, ctq_tot, cannabisuse, parental_psychosis_chk, 
                                                       parentssocialclass, socialclass, IQ_total, benton.total, benton.recoded, 
                                                       languagedistance, paternalage, maternalage, dfar.complete, dfar.all, dfar.angry, 
                                                       dfar.fear, dfar.happy, dfar.neutral, Hu.neutral, Hu.angry, Hu.fear, Hu.happy, 
                                                       misinterpretation.neutral, misinterpretation.happy, negativity.bias, p.happyneutral, 
                                                       p.happyangry, p.happyfear, p.angerfear, p.angerhappy, p.angerneutral, p.fearanger, 
                                                       p.fearhappy, p.fearneutral, p.neutralanger, p.neutralfear, p.neutralhappy,
                                                       symbol_coding_scaled))





# Check for missing values in preimputed dataset by subjectstatus 
table((is.na(data_preimp_filtered2$dfar.all)), data_preimp_filtered2$subjectstatus)
table((is.na(data_preimp_filtered2$age)), data_preimp_filtered2$subjectstatus)
table((is.na(data_preimp_filtered2$benton.total)), data_preimp_filtered2$subjectstatus)
table((is.na(data_preimp_filtered2$country)), data_preimp_filtered2$subjectstatus)
table((is.na(data_preimp_filtered2$gender)), data_preimp_filtered2$gender)
table((is.na(data_preimp_filtered2$ethnicity)), data_preimp_filtered2$ethnicity)
table((is.na(data_preimp_filtered2$negativity.bias)))
table((is.na(data_preimp_filtered2$IQ_total)), data_preimp_filtered2$subjectstatus)
table((is.na(data_preimp_filtered2$symbol_coding_scaled)), data_preimp_filtered2$subjectstatus)
table((is.na(data_preimp_filtered2$subjectstatus)))

table(data_preimp_filtered2$parentssocialclass, data_preimp_filtered2$ethnicity)

# Create a vector of cognitive variables to exclude
cognitive_vars <- c("symbol_coding_scaled", "symbol_coding_scaled_EU","symbol_coding_scaled_BR") # exclude these as predictors in the imputation so missingness does not reduce the sample

# Exclude cognitive vars as predictors for parentssocialclass
formula_impute <- as.formula(paste(". ~ . -", paste(cognitive_vars, collapse = " - "))) 

write.csv(data_preimp_filtered2, "2025_dataset_preimputed_filtered_sensitivity_analysis_IQ.csv")

data_preimp_filtered2 <- read.csv("2025_dataset_preimputed_filtered_sensitivity_analysis_IQ.csv")

###################################################################################

# missRanger multiple imputation

data_imputed <- missRanger(data = data_preimp_filtered2,
                           formula = formula_impute,
                           pmm.k = 3,
                           maxiter = 10L,
                           seed = 2022,
                           verbose = 1,
                           returnOOB = TRUE,
                           case.weights = NULL,
                           num.trees = 5000)

# Check appropriateness with out of bag errors 
attr(data_imputed, "oob") 

write.csv(data_imputed,"missRanger_postimputed_data_2025_sensitivity_analysis_IQ.csv")
data_imputed <- read.csv("missRanger_postimputed_data_2025_sensitivity_analysis_IQ.csv")

table(data_imputed$subjectstatus)
str(data_imputed$subjectstatus)
table(data_imputed$gender)
str(data_imputed$gender)
table(data_imputed$country, data_imputed$subjectstatus)
str(data_imputed$country)
table(data_imputed$ethnicity, data_imputed$subjectstatus)
str(data_imputed$ethnicity)
table(data_imputed$subjectstatus, data_imputed$parentssocialclass)
table(data_imputed$parentssocialclass)

table(is.na(data_preimp_filtered2$parentssocialclass))

##################################################################################

###  Descriptive statistics 

# Run Wilcoxon tests
wilcox.age <- wilcox.test(age ~ subjectstatus, data = data_imputed)
wilcox.bfrt <- wilcox.test(benton.total ~ subjectstatus, data = data_imputed)
wilcox.dfar <- wilcox.test(dfar.all ~ subjectstatus, data = data_imputed)
wilcox.dsc <- wilcox.test(symbol_coding_scaled ~ subjectstatus, data = data_imputed)
wilcox.IQ <- wilcox.test(IQ_total ~ subjectstatus, data = data_imputed)

# Extract W (Wilcoxon test statistic)
W.age <- wilcox.age$statistic
W.bfrt <- wilcox.bfrt$statistic
W.dfar <- wilcox.dfar$statistic 
W.dsc <- wilcox.dsc$statistic
W.IQ <- wilcox.IQ$statistic 

# Get sample sizes for each group in 'subjectstatus'
n1 <- sum(data_imputed$subjectstatus == unique(data_imputed$subjectstatus)[1])
n2 <- sum(data_imputed$subjectstatus == unique(data_imputed$subjectstatus)[2])

# Compute mean and standard deviation of W under the null hypothesis
mu_W <- (n1 * n2) / 2  # Mean of W
sigma_W <- sqrt((n1 * n2 * (n1 + n2 + 1)) / 12)  # Standard deviation of W

# Compute Z-scores for each test
Z.age <- (W.age - mu_W) / sigma_W
Z.bfrt <- (W.bfrt - mu_W) / sigma_W
Z.dfar <- (W.dfar - mu_W) / sigma_W
Z.dsc <- (W.dsc - mu_W) / sigma_W
Z.IQ <- (W.IQ - mu_W) / sigma_W


# Print Z-scores
Z.age
Z.bfrt
Z.dfar
Z.dsc
Z.IQ

wilcox.test(age ~ subjectstatus, data = data_imputed)
wilcox.test(benton.total ~ subjectstatus, data = data_imputed)
wilcox.test(dfar.all ~ subjectstatus, data = data_imputed)
wilcox.test(symbol_coding_scaled ~ subjectstatus, data = data_imputed)
wilcox.test(IQ_total ~ subjectstatus, data = data_imputed)

# Run chi-square tests

# Create a contingency table
table_sex <- table(data_imputed$gender, data_imputed$subjectstatus)
table_ethnicity <- table(data_imputed$ethnicity, data_imputed$subjectstatus)
table_country <- table(data_imputed$country, data_imputed$subjectstatus)
table_parentssocialclass <- table(data_imputed$parentssocialclass, data_imputed$subjectstatus)

# Run the Chi-squared test
chi_test_sex <- chisq.test(table_sex)
chi_test_ethnicity <- chisq.test(table_ethnicity)
chi_test_country <- chisq.test(table_country)
chi_test_parentssocialclass <- chisq.test(table_parentssocialclass)

# View the result
chi_test_sex
chi_test_ethnicity
chi_test_country
chi_test_parentssocialclass

table(data_imputed$subjectstatus)
cases <- subset(data_imputed, subjectstatus == "case")
controls <- subset(data_imputed, subjectstatus == "control")

summary(cases$dfar.all)
summary(controls$dfar.all)
summary(cases$benton.total)
summary(controls$benton.total)
summary(cases$age)
summary(controls$age)
summary(cases$symbol_coding_scaled)
summary(controls$symbol_coding_scaled)
summary(cases$block_design_scaled)
summary(controls$block_design_scaled)
summary(cases$information_scaled)
summary(controls$information_scaled)
summary(cases$arithmet_scaled)
summary(controls$arithmet_scaled)
summary(cases$IQ_total)
summary(controls$IQ_total)

tapply(cases$IQ_total, cases$ethnicity, summary)
tapply(controls$IQ_total, controls$ethnicity, summary)
tapply(cases$symbol_coding_scaled, cases$ethnicity, summary)
tapply(controls$symbol_coding_scaled, controls$ethnicity, summary)
tapply(cases$block_design_scaled, cases$ethnicity, summary)
tapply(controls$block_design_scaled, controls$ethnicity, summary)
tapply(cases$information_scaled, cases$ethnicity, summary)
tapply(controls$information_scaled, controls$ethnicity, summary)
tapply(cases$arithmet_scaled, cases$ethnicity, summary)
tapply(controls$arithmet_scaled, controls$ethnicity, summary)


table(data_imputed$country, data_imputed$subjectstatus)
table(data_imputed$ethnicity, data_imputed$subjectstatus)
table(data_imputed$gender, data_imputed$subjectstatus)
table(data_imputed$parentssocialclass, data_imputed$subjectstatus)


##################################################################################

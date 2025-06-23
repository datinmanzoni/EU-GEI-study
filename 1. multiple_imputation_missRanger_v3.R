# Clear existing workspace 
rm(list=ls())

# Load packages 
library(haven)
library(missRanger)
library(lme4)
library(dplyr)
library(ggplot2)

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

# Recode variables as factors
data_preimp$group_asian <- factor(data_preimp$group_asian, levels = c("0", "1"), labels = c("white", "asian")) 
data_preimp$gender <- factor(data_preimp$gender, levels = c("1","2"), labels = c("male", "female"))
data_preimp$ethnicity <- factor(data_preimp$ethnicity, levels = c("1","2","3","4","5","6"), labels = c('white', 'black', 'mixed', 'asian', 'north african', 'other'))
data_preimp$subjectstatus <- factor(data_preimp$subjectstatus, levels = c("0","1"), labels = c("control", "case"))
data_preimp$country <- factor(data_preimp$country, levels = c("1", "2", "3", "4", "5", "6"), labels = c("UK", "Holland", "Spain", "France", "Italy", "Brazil"))
data_preimp$languagedistance <- factor(data_preimp$languagedistance, levels = c("0", "1"), labels = c("no", "yes"))
data_preimp$parentssocialclass <- factor(data_preimp$parentssocialclass, levels = c("0", "1", "2", "3"), labels = c("salariat", "intermediate", "working class", "longterm unemployed"))
data_preimp$educationlevel <- factor(data_preimp$educationlevel, levels = c("1", "2", "3", "4", "5", "6"))
data_preimp$cannabisuse <- factor(data_preimp$cannabisuse, levels = c("0", "1"), labels = c("no", "yes"))
data_preimp$living <- factor(data_preimp$living, levels = c("0", "1"), labels = c("no", "yes"))
data_preimp$relationship <- factor(data_preimp$relationship, levels = c("0", "1"), labels = c("no", "yes"))
data_preimp$site <- as.factor(data_preimp$site)
data_preimp$ethnicitysitespecific <- as.factor(data_preimp$ethnicitysitespecific)
data_preimp$parental_psychosis_chk <- factor(data_preimp$parental_psychosis_chk, levels = c("0", "1"), labels = c("no", "yes")) 

# Checks 
table(data_preimp$group_asian) 
table(data_preimp$group_black)
table(data_preimp$group_mixed)
table(data_preimp$group_northafrican)
table(data_preimp$group_other)
table(data_preimp$subjectstatus)
table(data_preimp$gender)
table(data_preimp$ethnicity) 
table(data_preimp$country)
table(data_preimp$languagedistance)
table(data_preimp$parentssocialclass)
table(data_preimp$cannabisuse)
table(data_preimp$living)
table(data_preimp$relationship)
table(data_preimp$site)
table(data_preimp$ethnicitysitespecific)
table(data_preimp$parental_psychosis_chk)

# Create filtered dataframe 
data_preimp_filtered <- subset(data_preimp, select = c(subjectID, subjectstatus, ethnicity, age, gender, ethnicitysitespecific, 
                                                       ageofmigration, site, country, migrationstatus, age_firstcontact, educationlevel, 
                                                       edulvl, living, relationship, ctq_tot, cannabisuse, parental_psychosis_chk, 
                                                       parentssocialclass, socialclass, IQ, IQ.brazil, benton.total, benton.recoded, 
                                                       languagedistance, paternalage, maternalage, dfar.complete, dfar.all, dfar.angry, 
                                                       dfar.fear, dfar.happy, dfar.neutral, Hu.neutral, Hu.angry, Hu.fear, Hu.happy, 
                                                       misinterpretation.neutral, misinterpretation.happy, negativity.bias, p.happyneutral, 
                                                       p.happyangry, p.happyfear, p.angerfear, p.angerhappy, p.angerneutral, p.fearanger, 
                                                       p.fearhappy, p.fearneutral, p.neutralanger, p.neutralfear, p.neutralhappy))



# Check for item-level missing values in preimputed dataset
table((is.na(data_preimp_filtered$subjectstatus)))
table((is.na(data_preimp_filtered$dfar.all)), data_preimp_filtered$subjectstatus)
table((is.na(data_preimp_filtered$age)), data_preimp_filtered$subjectstatus)
table((is.na(data_preimp_filtered$benton.total)), data_preimp_filtered$subjectstatus)
table((is.na(data_preimp_filtered$parentssocialclass)), data_preimp_filtered$subjectstatus)
table((is.na(data_preimp_filtered$country)), data_preimp_filtered$subjectstatus)
table((is.na(data_preimp_filtered$gender)), data_preimp_filtered$gender)
table((is.na(data_preimp_filtered$negativity.bias)), data_preimp_filtered$negativity.bias)
table((is.na(data_preimp_filtered$ethnicity)), data_preimp_filtered$ethnicity)

# Check for sample-level missing values (cases vs controls) in preimputed dataset

data_preimp_filtered2 <- subset(data_preimp, select = c(subjectID, subjectstatus, ethnicity, age, gender, country,  
                                                        parentssocialclass, benton.total, dfar.all, dfar.angry, 
                                                        dfar.fear, dfar.happy, dfar.neutral, negativity.bias))

# Calculate percentage of missing data per row
data_preimp_filtered2$missing_percent <- rowMeans(is.na(data_preimp_filtered2)) * 100 #percentage of variables missing in each row

# Compare average % missing for cases vs controls
aggregate(missing_percent ~ subjectstatus, data = data_preimp_filtered2, FUN = mean) #mean percentage missing per group (cases vs controls)


write.csv(data_preimp_filtered, "2024_dataset_preimputed_filtered_v3.csv")

###################################################################################

# missRanger multiple imputation

data_imputed <- missRanger(data = data_preimp_filtered,
                           formula = . ~ .,
                           pmm.k = 3,
                           maxiter = 10L,
                           seed = 2022,
                           verbose = 1,
                           returnOOB = TRUE,
                           case.weights = NULL,
                           num.trees = 5000)

# Check appropriateness with out of bag errors 
attr(data_imputed, "oob") 

write.csv(data_imputed,"missRanger_postimputed_data_2025_v3.csv")

##################################################################################

data_postimp_filtered <- read.csv("missRanger_postimputed_data_2025_v3.csv")

###  Descriptive statistics 

# Run Wilcoxon tests
wilcox.age <- wilcox.test(age ~ subjectstatus, data = data_postimp_filtered)
wilcox.bfrt <- wilcox.test(benton.total ~ subjectstatus, data = data_postimp_filtered)
wilcox.dfar <- wilcox.test(dfar.all ~ subjectstatus, data = data_postimp_filtered)

# Extract W (Wilcoxon test statistic)
W.age <- wilcox.age$statistic
W.bfrt <- wilcox.bfrt$statistic
W.dfar <- wilcox.dfar$statistic 

# Get sample sizes for each group in 'subjectstatus'
n1 <- sum(data_postimp_filtered$subjectstatus == unique(data_postimp_filtered$subjectstatus)[1])
n2 <- sum(data_postimp_filtered$subjectstatus == unique(data_postimp_filtered$subjectstatus)[2])

# Compute mean and standard deviation of W under the null hypothesis
mu_W <- (n1 * n2) / 2  # Mean of W
sigma_W <- sqrt((n1 * n2 * (n1 + n2 + 1)) / 12)  # Standard deviation of W

# Compute Z-scores for each test
Z.age <- (W.age - mu_W) / sigma_W
Z.bfrt <- (W.bfrt - mu_W) / sigma_W
Z.dfar <- (W.dfar - mu_W) / sigma_W

# Print Z-scores
Z.age
Z.bfrt
Z.dfar

wilcox.test(age ~ subjectstatus, data = data_postimp_filtered)
wilcox.test(benton.total ~ subjectstatus, data = data_postimp_filtered)
wilcox.test(dfar.all ~ subjectstatus, data = data_postimp_filtered)

# Run chi-square tests

# Create a contingency table
table_sex <- table(data_postimp_filtered$gender, data_postimp_filtered$subjectstatus)
table_ethnicity <- table(data_postimp_filtered$ethnicity, data_postimp_filtered$subjectstatus)
table_country <- table(data_postimp_filtered$country, data_postimp_filtered$subjectstatus)
table_parentssocialclass <- table(data_postimp_filtered$parentssocialclass, data_postimp_filtered$subjectstatus)

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

table(data_postimp_filtered$subjectstatus)
cases <- subset(data_postimp_filtered, subjectstatus == "case")
controls <- subset(data_postimp_filtered, subjectstatus == "control")

summary(cases$dfar.all)
summary(controls$dfar.all)
summary(cases$benton.total)
summary(controls$benton.total)
summary(cases$age)
summary(controls$age)

table(data_postimp_filtered$country, data_postimp_filtered$subjectstatus)
table(data_postimp_filtered$ethnicity, data_postimp_filtered$subjectstatus)
table(data_postimp_filtered$gender, data_postimp_filtered$subjectstatus)
table(data_postimp_filtered$parentssocialclass, data_postimp_filtered$subjectstatus)

##################################################################################

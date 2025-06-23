# Clear existing workspace 
rm(list=ls())

# Set seed
set.seed(2022)

# Load imputed dataset
data_asian <- read.csv("data_asian_filtered_2024_v3.csv")

colnames(data_asian)
table(data_asian$group_asian)
str(data_asian$group_asian)
table(data_asian$subjectstatus)

# DFAR scores: asian

# Filter the data for group_asian == 1: cases and controls
asian_controls <- data_asian[data_asian$group_asian == "1" & data_asian$subjectstatus == "control", ]
asian_cases <- data_asian[data_asian$group_asian == "1" & data_asian$subjectstatus == "case", ]

# dfar.all
summary(asian_controls$dfar.all)
summary(asian_cases$dfar.all)

# dfar.neural
summary(asian_controls$dfar.neutral)
summary(asian_cases$dfar.neutral)

# dfar.happy
summary(asian_controls$dfar.happy)
summary(asian_cases$dfar.happy)

# dfar.fear
summary(asian_controls$dfar.fear)
summary(asian_cases$dfar.fear)

# dfar.angry
summary(asian_controls$dfar.angry)
summary(asian_cases$dfar.angry)

# Negativity bias: asian
summary(asian_controls$negativity.bias)
summary(asian_cases$negativity.bias)

# DFAR scores: white 

# Filter the data for group_asian == 0: cases and controls
white_controls <- data_asian[data_asian$group_asian == "0" & data_asian$subjectstatus == "control", ]
white_cases <- data_asian[data_asian$group_asian == "0" & data_asian$subjectstatus == "case", ]

# dfar.all
summary(white_controls$dfar.all)
summary(white_cases$dfar.all)

# dfar.neural
summary(white_controls$dfar.neutral)
summary(white_cases$dfar.neutral)

# dfar.happy
summary(white_controls$dfar.happy)
summary(white_cases$dfar.happy)

# dfar.fear
summary(white_controls$dfar.fear)
summary(white_cases$dfar.fear)

# dfar.angry
summary(white_controls$dfar.angry)
summary(white_cases$dfar.angry)

# Negativity bias: white
summary(white_controls$negativity.bias)
summary(white_cases$negativity.bias)

################################################################################

# DFAR scores: black 

# Load imputed dataset
data_black <- read.csv("data_black_filtered_2024_v3.csv")

# Filter the data for group_black == 1
black_controls <- data_black[data_black$group_black == "1" & data_black$subjectstatus == "control", ]
black_cases <- data_black[data_black$group_black == "1" & data_black$subjectstatus == "case", ]

# dfar.all
summary(black_controls$dfar.all)
summary(black_cases$dfar.all)

# dfar.neural
summary(black_controls$dfar.neutral)
summary(black_cases$dfar.neutral)

# dfar.happy
summary(black_controls$dfar.happy)
summary(black_cases$dfar.happy)

# dfar.fear
summary(black_controls$dfar.fear)
summary(black_cases$dfar.fear)

# dfar.angry
summary(black_controls$dfar.angry)
summary(black_cases$dfar.angry)

# Negativity bias: black
summary(black_controls$negativity.bias)
summary(black_cases$negativity.bias)


################################################################################

# DFAR scores: mixed 

# Load imputed dataset
data_mixed <- read.csv("data_mixed_filtered_2024_v3.csv")

# Filter the data for group_mixed == 1
mixed_controls <- data_mixed[data_mixed$group_mixed == "1" & data_mixed$subjectstatus == "control", ]
mixed_cases <- data_mixed[data_mixed$group_mixed == "1" & data_mixed$subjectstatus == "case", ]

# dfar.all
summary(mixed_controls$dfar.all)
summary(mixed_cases$dfar.all)

# dfar.neural
summary(mixed_controls$dfar.neutral)
summary(mixed_cases$dfar.neutral)

# dfar.happy
summary(mixed_controls$dfar.happy)
summary(mixed_cases$dfar.happy)

# dfar.fear
summary(mixed_controls$dfar.fear)
summary(mixed_cases$dfar.fear)

# dfar.angry
summary(mixed_controls$dfar.angry)
summary(mixed_cases$dfar.angry)

# Negativity bias: mixed
summary(mixed_controls$negativity.bias)
summary(mixed_cases$negativity.bias)

################################################################################

# DFAR scores: north african  

# Load imputed dataset
data_northafrican <- read.csv("data_northafrican_filtered_2024_v3.csv")

# Filter the data for group_northafrican == 1
northafrican_controls <- data_northafrican[data_northafrican$group_northafrican == "1" & data_northafrican$subjectstatus == "control", ]
northafrican_cases <- data_northafrican[data_northafrican$group_northafrican == "1" & data_northafrican$subjectstatus == "case", ]

# dfar.all
summary(northafrican_controls$dfar.all)
summary(northafrican_cases$dfar.all)

# dfar.neural
summary(northafrican_controls$dfar.neutral)
summary(northafrican_cases$dfar.neutral)

# dfar.happy
summary(northafrican_controls$dfar.happy)
summary(northafrican_cases$dfar.happy)

# dfar.fear
summary(northafrican_controls$dfar.fear)
summary(northafrican_cases$dfar.fear)

# dfar.angry
summary(northafrican_controls$dfar.angry)
summary(northafrican_cases$dfar.angry)

# Negativity bias: north african
summary(northafrican_controls$negativity.bias)
summary(northafrican_cases$negativity.bias)

################################################################################

# DFAR scores: other 

# Load imputed dataset
data_other <- read.csv("data_other_filtered_2024_v3.csv")

# Filter the data for group_other == 1
other_controls <- data_other[data_other$group_other == "1" & data_other$subjectstatus == "control", ]
other_cases <- data_other[data_other$group_other == "1" & data_other$subjectstatus == "case", ]

# dfar.all
summary(other_controls$dfar.all)
summary(other_cases$dfar.all)

# dfar.neural
summary(other_controls$dfar.neutral)
summary(other_cases$dfar.neutral)

# dfar.happy
summary(other_controls$dfar.happy)
summary(other_cases$dfar.happy)

# dfar.fear
summary(other_controls$dfar.fear)
summary(other_cases$dfar.fear)

# dfar.angry
summary(other_controls$dfar.angry)
summary(other_cases$dfar.angry)

# Negativity bias: other
summary(other_controls$negativity.bias)
summary(other_cases$negativity.bias)


################################################################################
# Parent–child interactions as mediators of adolescent outcomes
# Author: Lily X. Gates (Van Hout)

# ============================
# LOAD PACKAGES AND DATA
# ============================
# Load required packages
library(ggplot2)   # For graphing
library(jtools)    # For APA format
library(tidyverse) # For data management, includes dplyr

# Load PROCESS macro
source("./process.R")

# Read in and summarize data
lab_data <- read.csv("lab_data.csv")      # Outcome & PCI data
nle_data <- read.csv("NLE_data.csv")      # Negative life events data
lab_labels <- read.csv("lab_data_var_labels.csv")  # Lab variable labels
nle_labels <- read.csv("NLE_template_spreadsheet.csv")  # NLE variable labels

# ============================
# MERGING DATA
# ============================

# lab_data: ID is primary key
# nle_data: famid is primary key

lab_nle_data <- lab_data %>%
  inner_join(nle_data, by = c("ID" = "famid"))

# ============================
# RENAMING COLUMNS
# ============================

full_df <- lab_nle_data %>%
  rename(
    childs_sex = sex,
    internal_psyc = INTERNAL,
    external_psyc = EXTERNAL
  )

# ============================
# DROPPING COLUMNS
# ============================

# Drop columns: "X", "o1coder"
drop <- c("X", "o1coder")

filtered_df <- full_df[, !(names(full_df) %in% drop)]

# ============================
# CONVERT TO NUMERIC
# ============================

# Columns to exclude from numeric conversion
char_col <- c("ID", "childs_sex", "o1rpinscor", "o1rpinscom", "o1rpothprob")

# Convert all columns to numeric except those in char_col
filtered_df <- filtered_df %>%
  mutate(across(.cols = -all_of(char_col), .fns = ~as.numeric(.)))

# Identify columns not numeric and not in char_col
cols_to_convert <- setdiff(names(filtered_df)[!sapply(filtered_df, is.numeric)], char_col)

# Convert only those columns
filtered_df[cols_to_convert] <- lapply(filtered_df[cols_to_convert], as.numeric)

# ============================================
# Sample Description: Child Age and Gender
# ============================================

# Total sample size (non-missing age values)
N <- sum(!is.na(filtered_df$c1ageyr))
print("Sample Size"); print(N)

# Child age: min, max, mean, standard deviation
print("Minimum Value"); min(filtered_df$c1ageyr, na.rm = TRUE)
print("Maximum Value"); max(filtered_df$c1ageyr, na.rm = TRUE)
print("Mean Value"); mean(filtered_df$c1ageyr, na.rm = TRUE)
print("Standard Deviation"); sd(filtered_df$c1ageyr, na.rm = TRUE)

# ============================================
# Subset by Child Gender
# ============================================

# Males
males <- filtered_df[filtered_df$childs_sex == "Male", ]
n_males <- sum(!is.na(males$c1ageyr))
mean_males <- mean(males$c1ageyr, na.rm = TRUE)
sd_males <- sd(males$c1ageyr, na.rm = TRUE)
range_males <- range(males$c1ageyr, na.rm = TRUE)

cat("Male children (n =", n_males,"): Mean age =", round(mean_males, 2),
    "years, SD =", round(sd_males, 2),", Range =", round(range_males[1], 2),
    "-", round(range_males[2], 2), "years.\n")

# Females
females <- filtered_df[filtered_df$childs_sex == "Female", ]
n_females <- sum(!is.na(females$c1ageyr))
mean_females <- mean(females$c1ageyr, na.rm = TRUE)
sd_females <- sd(females$c1ageyr, na.rm = TRUE)
range_females <- range(females$c1ageyr, na.rm = TRUE)

cat("Female children (n =", n_females, "): Mean age =", round(mean_females, 2),
    "years, SD =", round(sd_females, 2), ", Range =", round(range_females[1], 2),
    "-", round(range_females[2], 2), "years.\n")

# ============================================
# Socioeconomic Status: Income-to-Needs (ITN)
# ============================================

mean_value <- mean(filtered_df$ITN, na.rm = TRUE)
range_values <- range(filtered_df$ITN, na.rm = TRUE)
sd_value <- sd(filtered_df$ITN, na.rm = TRUE)

cat("ITN - Mean:", round(mean_value, 2), "\n")
cat("ITN - Range:", round(range_values[1], 2), "to", round(range_values[2], 2), "\n")
cat("ITN - SD:", round(sd_value, 2), "\n")


# Multiple Mediation
# ============================================
# Sorting NLE into Vectors by Category
# ============================================
# Using "Theoretical Grouping", but could potentially use factorial analysis to analyze the latent variables. For example, Exploratory Data Analysis or Confirmatory Factor Analysis (Structural Equation Modeling).

# Define all groups in a named list
nle_groups <- list(
  trans = c("m1le019", "m1le022", "m1le023", "m1le024", "m1le025", "m1le026", "m1le027", "m1le028"),
  death = c("m1le018", "m1le029"),
  ill_injury = c("m1le002", "m1le003", "m1le004", "m1le008"),
  finances = c("m1le009", "m1le017"),
  psyc_stress = c("m1le005", "m1le010", "m1le011", "m1le012", "m1le020", "m1le021"),
  violence = c("m1le006", "m1le007"),
  behav_legal = c("m1le001", "m1le015", "m1le016")
)

# ============================================
# Predictor of NLE using "Total Severity"
# ============================================
# This will use "Total Severity," by using sum of ratings. Another approach could be "Total Number of Events" by summing any instances of non-zeros.

# ============================================
# Creating Columns with NLE Averages by Category
# ============================================

# Loop through the list and compute means
for(group_name in names(nle_groups)) {
  
  # Select the appropriate columns with the same name in the df
  cols <- nle_groups[[group_name]]
  
  # Check columns exist in df to avoid errors
  cols <- cols[cols %in% names(filtered_df)]
  
  # Rename the column in a format of nle_[group_name]_mean
  new_col_name <- paste0("nle_", group_name, "_mean")
  
  # Make the column contain an overall average of NLE
  filtered_df[[new_col_name]] <- rowMeans(filtered_df[cols], na.rm = TRUE)
}

#colnames(filtered_df)


# Create a Column with Average of All NLE

# Extract group names
group_names <- names(nle_groups)

# Find columns that match the naming pattern in the df
nle_mean_cols <- paste0("nle_", group_names, "_mean")
nle_mean_cols <- nle_mean_cols[nle_mean_cols %in% names(filtered_df)]

# Add column that contains average of all NLE; ignore NAs
filtered_df$nle_total_mean <- rowMeans(filtered_df[nle_mean_cols], na.rm = TRUE)

#filtered_df$nle_total_mean

# ============================================
# Predictor of NLE using "Total Number of Events"
# ============================================

# Calculated by summing any instances of non-zeros for NLE categories.**

# Loop through the list and compute total number of events
for(group_name in names(nle_groups)) {
  
  # Select the appropriate columns with the same name in the df
  cols <- nle_groups[[group_name]]
  
  # Check columns exist in df to avoid errors
  cols <- cols[cols %in% names(filtered_df)]
  
  # Rename the column in a format of nle_[group_name]_total_events
  new_col_name <- paste0("nle_", group_name, "_total_events")
  
  # Make the column contain total number of events (count non-zeros); ignore NAs
  filtered_df[[new_col_name]] <- rowSums(filtered_df[cols] != 0, na.rm = TRUE)
}

# Add column with total sum of all event occurrences

# Extract group names
group_names <- names(nle_groups)

# Find columns that match the naming pattern
nle_total_event_cols <- paste0("nle_", group_names, "_total_events")
nle_total_event_cols <- nle_total_event_cols[nle_total_event_cols %in% names(filtered_df)]

# Add column that contains total sum of events; ignore NAs
filtered_df$nle_all_total_events <- rowSums(filtered_df[nle_total_event_cols], na.rm = TRUE)


# Predictors Renamed
NLE_num_events <- filtered_df$nle_all_total_events
NLE_severity <- filtered_df$nle_total_mean

# ============================================
# Analyzing NLE number of events and severity
# ============================================

# For number of NLE events
mean_events <- mean(filtered_df$nle_all_total_events, na.rm = TRUE)
sd_events <- sd(filtered_df$nle_all_total_events, na.rm = TRUE)
range_events <- range(filtered_df$nle_all_total_events, na.rm = TRUE)

# For NLE severity
mean_severity <- mean(filtered_df$nle_total_mean, na.rm = TRUE)
sd_severity <- sd(filtered_df$nle_total_mean, na.rm = TRUE)
range_severity <- range(filtered_df$nle_total_mean, na.rm = TRUE)

# Print results
cat("NLE Number of Events - Mean:", mean_events, "SD:", sd_events, "Range:", range_events[1], "to", range_events[2], "\n")
cat("NLE Severity - Mean:", mean_severity, "SD:", sd_severity, "Range:", range_severity[1], "to", range_severity[2], "\n")

# ============================================
# Organizing Mediators
# ============================================

# Extract column names for relevant mediators
pci_vars <- c(
  "o1mpaa",    # Mom Positive Affect
  "o1mnaa",    # Mom Negative Affect
  "o1minta",   # Mom Interactiveness
  "o1mrata",   # Respect for Autonomy
  "o1mnca",    # Negative Control
  "o1mgsa",    # Guidance & Structuring
  "o1mlta",    # Limit Setting
  "o1mrespa",  # Responsiveness (corrected)
  "o1ccoa",    # Child Compliance
  "o1cnaa",    # Child Negative Affect
  "o1cpaa",    # Child Positive Affect
  "o1mncar",   # Negative Control Reversed
  "o1mscaf",   # Scaffolding
  "o1mwrm"     # Warmth
)

# User-Friendly Description of Mediator Variables

# Define a named vector for clearer output column names, variable name mapping
pci_domains <- c(
  mpaa  = "mom_pos_affect",
  mnaa  = "mom_neg_affect",
  minta = "mom_interactiveness",
  mrata = "mom_respect_autonomy",
  mnca  = "mom_neg_control",
  mgsa  = "mom_guidance_structuring",
  mlta  = "mom_limit_setting",
  mrespa = "mom_responsiveness",  # <-- corrected from mrspa
  ccoa  = "child_compliance",
  cnaa  = "child_neg_affect",
  cpaa  = "child_pos_affect",
  mncar = "mom_neg_control_reversed",
  mscaf = "mom_scaffolding",
  mwrm  = "mom_warmth"
)

# Loop to create new pci_ columns for GO1 only
# Convert to numeric
for (code in names(pci_domains)) {
  var_name <- paste0("o1", code)
  new_col <- paste0("pci_", pci_domains[[code]])
# If there is missing data, will show warning
  if (var_name %in% names(filtered_df)) {
    filtered_df[[new_col]] <- as.numeric(filtered_df[[var_name]])
  } else {
    warning(paste("Missing column:", var_name))
    filtered_df[[new_col]] <- NA
  }
}

#colnames(filtered_df)
#pci_domains

# ============================================
# Sort PCI by Category**
# ============================================

# Warmth
  # Latent: Positive Affect, Responsiveness
pci_warmth <- c("o1mpaa", "o1mrespa")

# Rejection
  # Latent: Negativity, Negative Control
pci_rejection <- c("o1mnaa", "o1mnca")

# Structure
  # Latent: Guidance, Autonomy, Compliance
pci_structure <- c("o1mgsa", "o1mrata", "o1ccoa")

# Interactiveness
  # Observed (standalone): Interactiveness score
pci_interactiveness <- "o1minta"


# Creating Columns for PCIs

# Compute average for warmth, rejection, structure
filtered_df$pci_warmth <- rowMeans(filtered_df[pci_warmth], na.rm = TRUE)
filtered_df$pci_rejection <- rowMeans(filtered_df[pci_rejection], na.rm = TRUE)
filtered_df$pci_structure <- rowMeans(filtered_df[pci_structure], na.rm = TRUE)

# Note: Not computing average because stand alone variable
filtered_df$pci_interactiveness <- as.numeric(filtered_df[[pci_interactiveness]])

# ============================================
# Organizing Covariate(s)
# ============================================
# Potential Covariates: Sex of the child (childs_sex), age of child (c1ageyr), mother's years of education (Mom_edu_cont), socioeconomic status/income-to-needs ratio (ITN)

# Coding for Child's Sex

# Find the unique variables in the childs_sex column
#unique(filtered_df$childs_sex)

# Convert childs_sex into factor variable

filtered_df$childs_sex <- factor(filtered_df$childs_sex, levels = c("Male", "Female"))

# Create logical binary coded column for child's sex
  # Female = 1, Male = 0
filtered_df$childs_sex_num <- ifelse(filtered_df$childs_sex == "Female", 1, 0)

# Convert logical to numeric
# PROCESS cannot handle factors or non-numeric (incl logical)
filtered_df$childs_sex_num <- as.numeric(filtered_df$childs_sex_num)

# ============================================
# Download 'filtered_df' as a .CSV
# ============================================

library(readr) # Needed to download an r object as CSV
write_csv(filtered_df, "complete_data_set.csv")

# ============================================
# Parallel Mediation Model
# ============================================

# Setting Up Parallel Mediation in PROCESS

#Checking data types and any missing values
#NOTE: Some cases with missing data were deleted. The number of deleted cases was: 11

# model = 4 is for simple or parallel mediation
# contrast = 2 is for difference in absolute values
# total = 1 shows total effect
# progress = 0 turns off bootstrap progress bar
# seed = 42 makes results reproducible
# boot = number of bootstrap samples

# PREPPING - Checking data types and any missing values

# All varibales being used for model
vars <- c(
  "nle_total_mean",
  "nle_all_total_events",
  "pci_mom_pos_affect",
  "pci_mom_neg_affect",
  "pci_mom_interactiveness",
  "pci_mom_respect_autonomy",
  "pci_mom_neg_control",
  "pci_mom_guidance_structuring",
  "pci_mom_limit_setting",
  "pci_mom_responsiveness",
  "pci_child_compliance",
  "pci_child_neg_affect",
  "pci_child_pos_affect",
  "pci_mom_neg_control_reversed",
  "pci_mom_scaffolding",
  "pci_mom_warmth",
  "pci_warmth",
  "pci_rejection",
  "pci_structure",
  "pci_interactiveness",
  "childs_sex_num"
)

# Check class types
#sapply(filtered_df[, vars], class)

# Check complete columns
complete_cases <- filtered_df[complete.cases(filtered_df[, vars]), ]
print("Complete Cases"); nrow(complete_cases)

# Checking missing values
colSums(is.na(filtered_df[, vars]))

# Rows with any NA in the vars columns
#missing_rows <- filtered_df[!complete.cases(filtered_df[, vars]), ]

# View missing rows
# Note: potentially because of audio/visual problems unable to code for PCIs
#missing_rows

# ============================================
## **Running the Models**
# ============================================

# ============================================
### **Internal Psychopathology**
# ============================================

# RUNNING - Internal Psychopathology

# Model 1 - Total Severity: Using nle_total_mean as the predictor for internal psychopathology
process(
  data = filtered_df,
  x = "nle_total_mean",
  y = "internal_psyc",
  m = c("pci_warmth", "pci_rejection", "pci_structure", "pci_interactiveness"),
  cov = c("childs_sex_num", "c1ageyr", "ITN"),
  model = 4,
  contrast = 2,
  seed = 42,
  boot = 5000,
  total = 1,
  progress = 0,
)

# Model 2 - Total Number of Events: Using nle_all_total_events as the predictor for internal psychopathology
process(
  data = filtered_df,
  x = "nle_all_total_events",
  y = "internal_psyc",
  m = c("pci_warmth", "pci_rejection", "pci_structure", "pci_interactiveness"),
  cov = c("childs_sex_num", "c1ageyr", "ITN"),
  model = 4,
  contrast = 2,
  seed = 42,
  boot = 5000,
  total = 1,
  progress = 0
)

# ============================================
### **External Psychopathology**
# ============================================

# RUNNING: External Psychopathology

# Model 3 - Total Severity & External Psycopathology: Using nle_total_mean as the predictor for external psychopathology
process(
  data = filtered_df,
  x = "nle_total_mean",
  y = "external_psyc",
  m = c("pci_warmth", "pci_rejection", "pci_structure", "pci_interactiveness"),
  cov = c("childs_sex_num", "c1ageyr", "ITN"),
  model = 4,
  contrast = 2,
  seed = 42,
  boot = 5000,
  total = 1,
  progress = 0,
)

# Model 4 - Total Number of Events & External Psycopathology: Using nle_all_total_events as the predictor for external psychopathology
process(
  data = filtered_df,
  x = "nle_all_total_events",
  y = "external_psyc",
  m = c("pci_warmth", "pci_rejection", "pci_structure", "pci_interactiveness"),
  cov = c("childs_sex_num", "c1ageyr", "ITN"),
  model = 4,
  contrast = 2,
  seed = 42,
  boot = 5000,
  total = 1,
  progress = 0
)



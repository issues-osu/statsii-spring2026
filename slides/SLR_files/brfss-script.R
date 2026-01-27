############################################################
# BRFSS 2021 ACEs Assignment Script
# Introductory Version with Clear Annotations (FINAL)
#
# What this script does:
# 1. Loads the BRFSS ACEs data
# 2. Creates an ACE cumulative risk score (0–11)
# 3. Describes the ACE score (mean, SD, range)
# 4. Computes Spearman correlations among ACEs
#    (excluding sexual abuse/assault items)
# 5. Runs simple linear regressions predicting:
#    - Poor mental health days
#    - Poor physical health days
#    - Binge drinking
############################################################

# ---------------------------
# Step 0: Load required packages
# ---------------------------

library(haven)
library(dplyr)
library(psych)

# ---------------------------
# Step 1: Load the SPSS data file
# ---------------------------

file_path <- "C:/Users/barboza-salerno.1/Documents/statistics-for-social-justice/stats-for-social-justice/slides/brfss_aces_subset.sav"
df <- read_sav(file_path)

head(df)

# ---------------------------
# Step 2: Define the 11 ACE variables
# ---------------------------
# IMPORTANT:
# In THIS file, ACE variables are already coded:
#   1 = Yes (ACE happened)
#   0 = No  (ACE did NOT happen)

ace_vars <- c(
  "ACEDEPRS",
  "ACEDIVRC",
  "ACEDRINK",
  "ACEDRUGS",
  "ACEHURT1",
  "ACEHVSEX",
  "ACEPRISN",
  "ACEPUNCH",
  "ACESWEAR",
  "ACETOUCH",
  "ACETTHEM"
)

# These 3 ACEs measure sexual abuse/assault.
# We EXCLUDE them from the correlation matrix.
sexual_abuse_vars <- c(
  "ACEHVSEX",
  "ACETOUCH",
  "ACETTHEM"
)

# Outcome variables
MENTAL_HEALTH <- "MENTHLTH"
PHYSICAL_HEALTH <- "PHYSHLTH"
BINGE_DRINKING <- "DRNK3GE5"

# ---------------------------
# Step 3: Create ACE cumulative risk score
# ---------------------------
# Because ACEs are already coded 0/1,
# we simply ADD them up.

df <- df %>%
  mutate(
    ace_sum = rowSums(across(all_of(ace_vars)), na.rm = TRUE)
  )

summary(df$ace_sum)

# ---------------------------
# Step 4: Describe the ACE sum score
# ---------------------------

ace_desc <- df %>%
  summarise(
    mean_ace_sum = mean(ace_sum, na.rm = TRUE),
    sd_ace_sum   = sd(ace_sum, na.rm = TRUE),
    min_ace_sum  = min(ace_sum, na.rm = TRUE),
    max_ace_sum  = max(ace_sum, na.rm = TRUE)
  )

cat("\n--- ACE Sum Descriptives ---\n")
print(ace_desc)

# ---------------------------
# Step 5: Spearman correlations among ACEs
# (excluding sexual abuse/assault ACEs)
# ---------------------------
# Spearman is a rank-based correlation.
# It is appropriate for binary/ordinal variables
# and does not assume normality.

ace_for_corr <- setdiff(ace_vars, sexual_abuse_vars)

corr_out <- corr.test(
  df %>% select(all_of(ace_for_corr)),
  use = "pairwise",
  method = "spearman"
)

cat("\n--- Spearman correlations among ACEs (excluding sexual abuse/assault) ---\n")
round(corr_out$r, 2)

# ---------------------------
# Step 6: Simple linear regression models
# ---------------------------

df$mh_days <- as.numeric(df[[MENTAL_HEALTH]])
df$ph_days <- as.numeric(df[[PHYSICAL_HEALTH]])
df$binge   <- as.numeric(df[[BINGE_DRINKING]])

m_mh <- lm(mh_days ~ ace_sum, data = df)
m_ph <- lm(ph_days ~ ace_sum, data = df)
m_bd <- lm(binge ~ ace_sum, data = df)

# ---------------------------
# Step 7: View and interpret model results
# ---------------------------

summary(m_mh)
summary(m_ph)
summary(m_bd)

# ---------------------------
# How to answer the assignment questions
# ---------------------------
# Intercept (b0): average outcome when ACE sum = 0
# Slope (b1): change in outcome for each additional ACE
# Null hypothesis: b1 = 0 (no relationship)
# R-squared: proportion of variance explained by ACEs
# Remember: This is observational, not causal.
############################################################

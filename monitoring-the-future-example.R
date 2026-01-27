# -------------------------------------------------------------------------
# Our First R Script
# Monitoring the Future 2023 (ICPSR 39445)
# Selected survey variables for neighborhood safety, school climate, and
# demographic background.
# -------------------------------------------------------------------------
# Here are the variables I selected:
# V8477
# During the past 12 months, how often have you seen people selling
# illegal drugs in your neighborhood?
# 1 = Never
# 2 = A few times a year
# 3 = Once or twice a month
# 4 = At least once a week
# 5 = Almost every day
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# V7535
# How often do you feel unsafe going to or from school?
# 1 = Never
# 2 = Rarely
# 3 = Some days
# 4 = Most days
# 5 = Every day
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# V7536
# During the last 4 weeks, how many days did you not go to school because
# you felt unsafe at school or on your way to or from school?
# 1 = 0 days
# 2 = 1 day
# 3 = 2 or 3 days
# 4 = 4 or more days
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# V8517
# During the last 12 months, how often have you taken part in a group fight
# where your friends were against another group?
# 1 = Not at all
# 2 = Once
# 3 = Twice
# 4 = 3 or 4 times
# 5 = 5 or more times
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# V8516
# During the last 12 months, how often have you gotten into a serious fight
# in school or at work?
# 1 = Not at all
# 2 = Once
# 3 = Twice
# 4 = 3 or 4 times
# 5 = 5 or more times
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# V7389
# In which competitive sports (if any) did you participate during the last
# 12 months? Includes school, community, and organized sports.
# Example category shown: Football
# 0 = Not marked
# 1 = Marked
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# V7221
# Which best describes your average grades this school year?
# 9 = A (93–100)
# 8 = A– (90–92)
# 7 = B+ (87–89)
# 6 = B (83–86)
# 5 = B– (80–82)
# 4 = C+ (77–79)
# 3 = C (73–76)
# 2 = C– (70–72)
# 1 = D (69 or below)
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# V1252
# Age classification based on birth year/month and questionnaire date.
# Included for Grade 10 dataset only.
# 1 = Younger than 16
# 2 = 16 years of age or older
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# V7202
# What is your sex?
# 1 = Male
# 2 = Female
# 3 = Other
# 4 = Prefer not to answer
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# V1070
# Recoded race/ethnicity.
# 1 = Black or African American
# 2 = White (Caucasian)
# 3 = Hispanic
# All other or multiple race responses are coded as missing.
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# V7215
# Highest level of schooling your father completed.
# 1 = Grade school or less
# 2 = Some high school
# 3 = Completed high school
# 4 = Some college
# 5 = Completed college
# 6 = Graduate or professional school
# 7 = Don't know / Does not apply
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# V7216
# Highest level of schooling your mother completed.
# 1 = Grade school or less
# 2 = Some high school
# 3 = Completed high school
# 4 = Some college
# 5 = Completed college
# 6 = Graduate or professional school
# 7 = Don't know / Does not apply
# -------------------------------------------------------------------------


vars <- c(
  "V8477", "V7535", "V7536", "V8517", "V8516",
  "V7389", "V7221", "V1252", "V7202", "V1070",
  "V7215", "V7216", "V7639"
)

# -------------------------------------------------------------------------
# Load Required Package
# -------------------------------------------------------------------------
library(haven)
library(dplyr)

# -------------------------------------------------------------------------
# Load the Monitoring the Future dataset
# -------------------------------------------------------------------------
df <- read_sav("C:/Users/barboza-salerno.1/Downloads/ED injury/ICPSR_39445-V2/ICPSR_39445/DS0001/39445-0001-Data.sav")

# -------------------------------------------------------------------------
# Select only the variables of interest
# -------------------------------------------------------------------------
df <- df %>%
  dplyr::select(
    V8477, V7535, V7536, V8517, V8516,
    V7389, V7221, V1252, V7202, V1070,
    V7215, V7216, V7639
  )

df <- df %>%
  mutate(across(everything(), ~ na_if(.x, -8))) %>%
  mutate(across(everything(), ~ na_if(.x, -9)))

# -------------------------------------------------------------------------
# Quick check
# -------------------------------------------------------------------------
summary(df)
dim(df)

# -------------------------------------------------------------------------
# Frequency Tables for All Variables, Check against codebook
# Note: both of these two analyses below give the same result
# -------------------------------------------------------------------------

for (v in names(df)) {
  cat("\n-----------------------------------------\n")
  cat("Variable:", v, "\n")
  print(table(df[[v]], useNA = "ifany"))
}

table(df$V1252, useNA = "ifany")
table(df$V7202, useNA = "ifany")
table(df$V1070, useNA = "ifany")
table(df$V7215, useNA = "ifany")
table(df$V7216, useNA = "ifany")

# -------------------------------------------------------------------------
# Recode concussion as binary: 0 = No, 1 = Ever had a concussion
# -------------------------------------------------------------------------
df$Concussion_Ever <- ifelse(
  df$V7639 == 1, 0,
  ifelse(df$V7639 %in% c(2,3), 1, NA))

# -------------------------------------------------------------------------
# Check the two variables
# -------------------------------------------------------------------------
table(df$V7389, useNA = "ifany")        # Football participation
table(df$Concussion_Ever, useNA = "ifany")

# -------------------------------------------------------------------------
# Cross-tabulation
# -------------------------------------------------------------------------
tab <- table(df$V7389, df$Concussion_Ever)
tab
prop.table(tab, margin = 1) * 100
# -------------------------------------------------------------------------
# Chi-square test of association
# -------------------------------------------------------------------------
chisq.test(tab)

# Calculate odds ratio: (285/673) / (818/3733)
# Students who reported playing football had about 1.9 times the odds of having been diagnosed with a concussion compared to students who did not play football.
# In other words, the odds of concussion were nearly twice as high among football participants.
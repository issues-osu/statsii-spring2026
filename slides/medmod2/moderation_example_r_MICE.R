if(!require(devtools)) install.packages("devtools")
devtools::install_github("cardiomoon/processR")

# Load Packages

library(haven)
library(processR)
library(ggplot2)
library(dplyr)
library(mice)

# Load Data

data <- read_sav("slides/medmod2/pts vex mod mediation.sav")

# Select variables for analysis

data_sub <- data %>%
  select(tra1, cidi1, bcdel1, negever)

# Remove labelled class from SPSS import

data_sub <- data_sub %>%
  mutate(across(where(haven::is.labelled), haven::zap_labels))

# Check missing data BEFORE imputation

colSums(is.na(data_sub))

md.pattern(data_sub)

# Convert binary variable to factor (required for logreg)

data_sub$negever <- factor(data_sub$negever, levels = c(0, 1))

# Set imputation methods

meth <- make.method(data_sub)
meth["tra1"] <- "pmm"
meth["cidi1"] <- "pmm"
meth["bcdel1"] <- "pmm"
meth["negever"] <- "logreg"

# Run MICE

imp <- mice(data_sub, m = 5, method = meth, seed = 123)

# Check missing data AFTER imputation

completed_data <- complete(imp, 1)

colSums(is.na(completed_data))

# Run moderation model on imputed data

fit_imp <- with(imp, lm(bcdel1 ~ tra1 * negever))

pooled <- pool(fit_imp)

summary(pooled)

# Extract pooled coefficients

coef(summary(pooled))

# Create interaction plot using imputed data

plot_data <- complete(imp, "long")

ggplot(plot_data,
       aes(x = tra1, y = bcdel1, color = negever)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Moderation of Trauma on Delinquent Behavior by Neglect (Imputed Data)",
    x = "Trauma (tra1)",
    y = "Delinquent Behavior (bcdel1)",
    color = "Neglect"
  ) +
  theme_minimal(base_size = 14)
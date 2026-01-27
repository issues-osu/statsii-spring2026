# Load necessary libraries
library(foreign)    # For reading .sav files
library(ggplot2)    # For visualization
library(dplyr)      # For data manipulation
library(gtsummary)  # For creating regression tables
library(officer)    # For exporting to Word
library(flextable)  # For table formatting

# Read the .sav file
admissions <- read.spss("data/admissions.sav", to.data.frame = TRUE)

# Inspect the first few rows to ensure proper loading
head(admissions)

# Select relevant variables and omit missing values listwise
# dplyr::select() extracts the required variables, and na.omit() removes rows with missing values
admissions_clean <- admissions %>% 
  dplyr::select(
    SAT_math, 
    SAT_verbal, 
    HS_class_size) %>% 
  na.omit()

# Simple Linear Regression: SAT_math ~ SAT_verbal
# This model predicts SAT_math using SAT_verbal as a predictor
simple_model <- lm(
  SAT_math ~ SAT_verbal, 
  data = admissions_clean)
summary(simple_model) # Display regression summary

# Compute fitted values and residuals
# fitted() extracts the predicted values, residuals() calculates actual - predicted
admissions_clean$fitted_simple <- fitted(simple_model)
admissions_clean$residuals_simple <- residuals(simple_model)

# Compute standardized fitted values and residuals
# scale() standardizes values by centering and scaling
admissions_clean$std_fitted_simple <- scale(admissions_clean$fitted_simple)
admissions_clean$std_residuals_simple <- scale(admissions_clean$residuals_simple)

# Plot standardized residuals vs. standardized fitted values
# aes() maps data, geom_point() creates scatter plot, geom_hline() adds reference line
ggplot(
  admissions_clean, 
  aes(
    x = std_fitted_simple, 
    y = std_residuals_simple)
) +
  geom_point(color = "blue") +
  geom_hline(
    yintercept = 0, 
    linetype = "dashed", 
    color = "red"
  ) +
  labs(
    title = "Standardized Residuals vs. Standardized Fitted Values (Simple Model)",
    x = "Standardized Fitted Values", 
    y = "Standardized Residuals"
  )

# Plot histogram of residuals
# geom_histogram() creates a histogram with specified bins and colors
ggplot(
  admissions_clean, 
  aes(
    x = residuals_simple)
) +
  geom_histogram(
    bins = 30, 
    fill = "lightblue", 
    color = "black"
  ) +
  labs(
    title = "Histogram of Residuals (Simple Model)", 
    x = "Residuals", 
    y = "Frequency"
  )

# Compute correlations
# cor() computes the correlation matrix
# [, c(...)] selects the specified columns for correlation calculation
cor(
  admissions_clean[, c(
    "SAT_math", 
    "SAT_verbal", 
    "fitted_simple", 
    "residuals_simple")
  ], 
  use = "complete.obs"
)

# Multiple Linear Regression: SAT_math ~ SAT_verbal + HS_class_size
# This model predicts SAT_math using SAT_verbal and HS_class_size
multiple_model <- lm(
  SAT_math ~ SAT_verbal + HS_class_size, 
  data = admissions_clean)
summary(multiple_model) # Display regression summary

# Compute fitted values and residuals
admissions_clean$fitted_multiple <- fitted(multiple_model)
admissions_clean$residuals_multiple <- residuals(multiple_model)

# Compute standardized fitted values and residuals
admissions_clean$std_fitted_multiple <- scale(admissions_clean$fitted_multiple)
admissions_clean$std_residuals_multiple <- scale(admissions_clean$residuals_multiple)

# Plot standardized residuals vs. standardized fitted values for multiple regression
ggplot(
  admissions_clean, 
  aes(
    x = std_fitted_multiple, 
    y = std_residuals_multiple)
) +
  geom_point(color = "purple") +
  geom_hline(
    yintercept = 0, 
    linetype = "dashed", 
    color = "red"
  ) +
  labs(
    title = "Standardized Residuals vs. Standardized Fitted Values (Multiple Model)",
    x = "Standardized Fitted Values", 
    y = "Standardized Residuals"
  )

# Plot histogram of residuals for multiple regression
ggplot(
  admissions_clean, 
  aes(
    x = residuals_multiple)
) +
  geom_histogram(
    bins = 30, 
    fill = "lightgreen", 
    color = "black"
  ) +
  labs(
    title = "Histogram of Residuals (Multiple Model)", 
    x = "Residuals", 
    y = "Frequency"
  )

# Compute correlations including the new predictor
cor(
  admissions_clean[, c(
    "SAT_math", 
    "SAT_verbal", 
    "HS_class_size", 
    "fitted_multiple", 
    "residuals_multiple")
  ], 
  use = "complete.obs"
)

# Create a visualization of SAT_verbal effect
ggplot(
  admissions_clean, 
  aes(
    x = SAT_verbal, 
    y = SAT_math)
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", col = "blue") +
  labs(
    title = "Effect of SAT Verbal on SAT Math", 
    x = "SAT Verbal", 
    y = "SAT Math"
  )

# Generate regression summary table
# tbl_regression() creates a formatted summary table, modify_*() functions customize headers and captions.
mlr_table <- multiple_model %>% 
  tbl_regression(
    label = list(
      SAT_verbal = "SAT Verbal Score", 
      HS_class_size = "High School Class Size"
    ),
    include = everything()
  ) %>%
  modify_header(
    label = "Variable"
  ) %>%
  modify_caption(
    "Table 1: Multiple Linear Regression Results"
  ) %>%
  add_n() %>%
  add_glance_source_note()

# Convert to flextable for better Word formatting
mlr_flextable <- as_flex_table(mlr_table)

# Save table to Word
doc <- read_docx()
doc <- body_add_flextable(
  doc, 
  value = mlr_flextable
)
print(
  doc, 
  target = "MLR_Results.docx"
)

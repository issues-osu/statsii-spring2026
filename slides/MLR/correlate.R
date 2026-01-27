# Load necessary libraries
library(tidyverse)
library(ppcor)  # For partial and semi-partial correlation
library(knitr)

# Load dataset
df <- read.csv("data/KSPRAMS_SUB_COR.csv")

# Select relevant columns
data_subset <- df %>% 
  dplyr::select(ACEs, STRESS, Meduc) %>% 
  na.omit()

# Check summary statistics
summary(data_subset)

# Compute Pearson correlation matrix
cor_matrix <- cor(data_subset, method = "pearson")

# Print correlation matrix
print("Pearson Correlation Matrix:")
print(cor_matrix)

# Compute Partial Correlation (controlling for ACEs)
partial_corr <- pcor(data_subset)
print("Partial Correlation Matrix:")
print(partial_corr$estimate)

# Compute Semi-Partial Correlation (controlling for ACEs only in STRESS)
semi_partial_corr <- spcor(data_subset)
print("Semi-Partial Correlation Matrix:")
print(semi_partial_corr$estimate)
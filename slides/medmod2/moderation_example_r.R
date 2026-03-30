if(!require(devtools)) install.packages("devtools")
devtools::install_github("cardiomoon/processR")

# Load Packages

library(haven)      # read SPSS files
library(processR)   # PROCESS-style models
library(ggplot2)
library(dplyr)
library(interactions) # JN

# Load Data

data <- read_sav("slides/medmod2/pts vex mod mediation.sav")

# PROCESS Model 1 (Moderation) in R using processR approach
# Dataset: pts vex mod mediation.SAV

# Generate PROCESS-style model

equations <- regEquation(
  X = "tra1",
  Y = "bcdel1",
  moderator = list(name = "negever", site = list("c"))
)

cat(equations)

# This should print:
# bcdel1 ~ tra1 + negever + tra1*negever

# Fit the model (PROCESS equivalent)

fit <- lm(as.formula(equations), data = data)

summary(fit)

# PROCESS-style summary table

modelsSummary(
  list(fit),
  labels = list(
    X = "tra1",
    Y = "bcdel1",
    W = "negever"
  )
)

# Extract coefficients
coefs <- coef(fit)

b0 <- coefs[1]                  # intercept
b1 <- coefs["tra1"]             # trauma
b2 <- coefs["negever"]          # neglect
b3 <- coefs["tra1:negever"]     # interaction

# Create predicted values
tra_seq <- seq(min(data$tra1, na.rm = TRUE),
               max(data$tra1, na.rm = TRUE),
               length.out = 100)

plot_data <- expand.grid(
  tra1 = tra_seq,
  negever = c(0, 1)
)

# Apply PROCESS model equation
plot_data$pred <- b0 +
  b1 * plot_data$tra1 +
  b2 * plot_data$negever +
  b3 * plot_data$tra1 * plot_data$negever

# Label groups for plotting
plot_data$negever <- factor(plot_data$negever,
                            levels = c(0, 1),
                            labels = c("No Neglect", "Neglect"))

# Plot interaction
ggplot(plot_data,
       aes(x = tra1, y = pred, color = negever)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Moderation of Trauma on Delinquent Behavior by Neglect",
    x = "Trauma (tra1)",
    y = "Predicted Delinquent Behavior (bcdel1)",
    color = "Neglect Status"
  ) +
  theme_minimal(base_size = 14)

####################################### Example 2 (W = internalizing CIDI scores)

# Extend to continuous moderator (cidi1)

equations_cidi <- regEquation(
  X = "tra1",
  Y = "bcdel1",
  moderator = list(name = "cidi1", site = list("c"))
)

cat(equations_cidi)

# This should print:
# bcdel1 ~ tra1 + cidi1 + tra1*cidi1

# Fit the model

fit_cidi <- lm(as.formula(equations_cidi), data = data)

summary(fit_cidi)

# PROCESS-style summary table

modelsSummary(
  list(fit_cidi),
  labels = list(
    X = "tra1",
    Y = "bcdel1",
    W = "cidi1"
  )
)

# Extract coefficients

coefs_cidi <- coef(fit_cidi)

b0 <- coefs_cidi[1]                 
b1 <- coefs_cidi["tra1"]            
b2 <- coefs_cidi["cidi1"]           
b3 <- coefs_cidi["tra1:cidi1"]      

# Conditional effect of X on Y
# θ = b1 + b3*W

w_vals <- quantile(data$cidi1, probs = c(.16, .50, .84), na.rm = TRUE)

cond_effects <- data.frame(
  cidi1 = as.numeric(w_vals),
  effect = b1 + b3 * as.numeric(w_vals)
)

cond_effects

# Create predicted values

tra_seq <- seq(min(data$tra1, na.rm = TRUE),
               max(data$tra1, na.rm = TRUE),
               length.out = 100)

plot_data_cidi <- expand.grid(
  tra1 = tra_seq,
  cidi1 = as.numeric(w_vals)
)

# Apply PROCESS model equation

plot_data_cidi$pred <- b0 +
  b1 * plot_data_cidi$tra1 +
  b2 * plot_data_cidi$cidi1 +
  b3 * plot_data_cidi$tra1 * plot_data_cidi$cidi1

# Label groups

plot_data_cidi$cidi_group <- factor(plot_data_cidi$cidi1,
                                    labels = c("Low Distress", "Moderate", "High Distress"))

# Plot interaction

ggplot(plot_data_cidi,
       aes(x = tra1, y = pred, color = cidi_group)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Moderation of Trauma by Internalizing Distress",
    x = "Trauma (tra1)",
    y = "Predicted Delinquent Behavior (bcdel1)",
    color = "Internalizing Distress"
  ) +
  theme_minimal(base_size = 14)


# Johnson-Neyman analysis (continuous moderator only)

jn <- johnson_neyman(
  fit_cidi,
  pred = tra1,
  modx = cidi1,
  alpha = 0.05
)

jn



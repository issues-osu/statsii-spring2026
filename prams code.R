library(survey)
library(haven)
library(nnet)
library(jtools)
library(interactions)
library(car)
library(dplyr)

data <- read.csv("C:/Users/barboza-salerno.1/Downloads/KSPRAMS_SUB_WEIGHT_ANALYSIS.csv")  # Replace with your file path
# Define the survey design object

# Assuming 'survey_design' is your survey design object

# List of categorical variables
categorical_vars <- c("Race_Ethnicity", "MARRIED", "P_PRTERM", "PAT_ACK", 
                      "BPG_DIAB8", "BPG_HBP8", "BPG_DEPRS8", "HTH_ANX", 
                      "NCHS_URB_RUR2", "AIntent")

# List of categorical variables
recodedvars <- c("MARRIED", "P_PRTERM", "PAT_ACK", 
                      "BPG_DIAB8", "BPG_HBP8", "BPG_DEPRS8", "HTH_ANX", 
                      "NCHS_URB_RUR2", "AIntent")


data$Abuse_Trajectory_c <- factor(
  data$Abuse_Trajectory, 
  levels = c(0, 1, 2), 
  labels = c("No abuse", "Abuse during & after", "Abuse only during"))

data <- data %>%
  mutate(across(all_of(recodedvars), as.factor))

data <- data %>%
  mutate(across(all_of(recodedvars), ~ if_else(
    as.numeric(as.character(.)) == 1, 0,
    if_else(as.numeric(as.character(.)) == 2, 1, as.numeric(as.character(.)))
  )))

# Recode 'AIntent' variable
data <- data %>%
  dplyr::mutate(
    AIntent = dplyr::recode(AIntent,
                            `1` = "LATER",
                            `2` = "SOONER",
                            `3` = "THEN",
                            `4` = "DID NOT WANT THEN OR ANY TIME",
                            `5` = "WAS NOT SURE",
                            .default = "BLANK"
    ))


survey_design <- svydesign(
  id = ~1,  # Use cluster variable if available, otherwise use ~1
  strata = ~ASTRATUMC,  # Strata variable
  weights = ~AWTANAL,   # Weight variable
  data = data
)
svyttest(MAT_AGE_PU ~ Any_Abuse, design = survey_design)
svyttest(AINCOME8 ~ Any_Abuse, design = survey_design)
svyttest(ACEs_Scale ~ Any_Abuse, design = survey_design)
svyttest(CIG_1TRI ~ Any_Abuse, design = survey_design)
svyttest(MAT_ED ~ Any_Abuse, design = survey_design)
svyttest(STRS_TT3 ~ Any_Abuse, design = survey_design)
svyttest(AMOM_BMI_BC ~ Any_Abuse, design = survey_design)
svyttest(MATHARDSHIP ~ Any_Abuse, design = survey_design)

svychisq(~Abuse_Trajectory + Race_Ethnicity, survey_design)
weighted_table <- svytable(~Abuse_Trajectory + MARRIED, survey_design)
(prevalence <- prop.table(weighted_table, margin = 2) * 100)

svychisq(~Abuse_Trajectory + MARRIED, survey_design)
svychisq(~Abuse_Trajectory + P_PRTERM, survey_design)
svychisq(~Abuse_Trajectory + PAT_ACK, survey_design)
svychisq(~Abuse_Trajectory + BPG_DIAB8, survey_design)
svychisq(~Abuse_Trajectory + BPG_HBP8, survey_design)
svychisq(~Abuse_Trajectory + BPG_DEPRS8, survey_design)
svychisq(~Abuse_Trajectory + HTH_ANX, survey_design)
svychisq(~Abuse_Trajectory + NCHS_URB_RUR2, survey_design)
svychisq(~Abuse_Trajectory + AIntent, survey_design)


svyciprop(~I(Abuse_Trajectory_c=="No abuse"), survey_design, method="likelihood")
svyciprop(~I(Abuse_Trajectory_c=="Abuse during & after"), survey_design, method="likelihood")
svyciprop(~I(Abuse_Trajectory_c=="Abuse only during"), survey_design, method="likelihood")

calculate_proportions_ci <- function(variable) {
  formula <- as.formula(paste("~", variable))
  svy_result <- svyby(
    formula,
    ~Abuse_Trajectory_c,
    design = survey_design,
    FUN = svymean,
    na.rm = TRUE,
    vartype = c( "ci")
  )
  return(svy_result)
}

# Apply the function to each categorical variable
results_list <- lapply(categorical_vars, calculate_proportions_ci)

# Display the results
results_list

########################
calculate_means_ci <- function(variable) {
  # Ensure the variable is numeric
  if (!is.numeric(survey_design$variables[[variable]])) {
    stop(paste("The variable", variable, "is not numeric. Convert it to numeric before proceeding."))
  }
  
  # Define the formula for the continuous variable
  formula <- as.formula(paste("~", variable))
  
  # Compute the weighted mean and confidence intervals with error handling
  svy_result <- tryCatch({
    svyby(
      formula,
      ~Abuse_Trajectory,
      design = survey_design,
      FUN = svymean,
      na.rm = TRUE,
      vartype = "ci"
    )
  }, error = function(e) {
    stop(paste("Error in svyby for variable", variable, ":", e$message))
  })
  
  return(svy_result)
}

# List of continuous variables
continuous_vars <- c("ACEs_Scale", "AINCOME8", "CIG_1TRI", "MAT_AGE_PU", "MATHARDSHIP",
                     "AINCOME8","MAT_ED", "STRS_TT3", "AMOM_BMI_BC")  # Replace with actual variable names

# Compute weighted means and confidence intervals for all continuous variables
results_list <- lapply(continuous_vars, function(var) {
  if (!var %in% names(survey_design$variables)) {
    stop(paste("Variable", var, "not found in survey design."))
  }
  calculate_means_ci(var)
})

# Display the results
results_list


################################
model_multinom <- multinom(Abuse_Trajectory ~ ACEs_Scale , 
                           data = data, 
                           weights = data$AWTANAL)
summary(model_multinom)
exp(coef(model_multinom))  # Convert log-odds to odds ratios

z_values <- summary(model_multinom)$coefficients / summary(model_multinom)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2  # Two-tailed test
p_values
predict(model_multinom, type = "probs")  # Predicted probabilities for each abuse category

model_interaction <- multinom(Abuse_Trajectory ~ ACEs_Scale * MATHARDSHIP , 
                              data = data, 
                              weights = data$AWTANAL)
summary(model_interaction)
z_values <- summary(model_interaction)$coefficients / summary(model_interaction)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2  # Two-tailed test
p_values

library(interactions)
model <- multinom(Abuse_Trajectory ~ ACEs_Scale * MATHARDSHIP, data = data)

model <- glm(Any_Abuse ~ ACEs_Scale * MATHARDSHIP, data = data, family = binomial(), 
             weights = data$AWTANAL)

interact_plot(model, pred = "ACEs_Scale", modx = "MATHARDSHIP", plot.jn = TRUE)
johnson_neyman(model, pred = "ACEs_Scale", modx = "MATHARDSHIP")

model <- glm(Any_Abuse ~ ACEs_Scale * MATHARDSHIP, data = data, family = quasibinomial(), 
             weights = data$AWTANAL)


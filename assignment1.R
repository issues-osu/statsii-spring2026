library(dplyr)
data <- read.csv("C:/Users/barboza-salerno.1/Downloads/KSPRAMS_SUB_WEIGHT_ANALYSIS.csv")  # Replace with your file path

data <- data %>% dplyr::select(CIG_1TRI, BPG_DEPRS8, Any_Abuse, ACEs_Scale, Race_Ethnicity) %>% na.omit()

# How many cases are omitted

# Describe the characteristics of respondents, in other words, create a descriptive table of variables for missing respondents

model0 <- lm(CIG_1TRI ~ Any_Abuse, data = data)
summary(model0)

model1 <- lm(CIG_1TRI ~ Any_Abuse + ACEs_Scale, data = data)
summary(model1)

model2 <- lm(CIG_1TRI ~ Any_Abuse + ACEs_Scale + BPG_DEPRS8, data = data)
summary(model2)

# How many people identify as Hispanic, NH Black, NH White and Other, make a simple table

# As you can see, the race/ethnicity data is categorical. Create dummy variables for Hispanic, NH Black, NH White, and Other
# An 'easy' way to do that is to use the package 'fastDummies' :)

#install.packages("fastDummies")  # Only if you haven't installed it yet
library(fastDummies)
df_dummies <- dummy_cols(data, select_columns = "Race_Ethnicity", remove_first_dummy = TRUE) 

# Explain the above line of code in full detail (i.e., explain what all words are referring to)

model3 <- lm(CIG_1TRI ~ Any_Abuse + ACEs_Scale + BPG_DEPRS8 + `Race_Ethnicity_NH White`  + `Race_Ethnicity_NH Black` + Race_Ethnicity_Other, data = df_dummies)
summary(model3)

# Notice that the levels of the categorical variable are 1) HIspanic; 2) BH Black; 3) NH White; and 4) Other
# There are many times when I want to 'relevel' a variable
# For example, I want to have NH Whites as the first level

data$Race_Ethnicity <- relevel(factor(data$Race_Ethnicity), ref = "NH White")
# Check the levels, you should see NH White first (hint: make a table of Race/Ethnicity)

df_dummies <- dummy_cols(data, select_columns = "Race_Ethnicity", remove_first_dummy = TRUE) 

# How did that change the coding?

# Include the race variables in the model
model3 <- lm(CIG_1TRI ~ Any_Abuse + ACEs_Scale + BPG_DEPRS8 + Race_Ethnicity_Hispanic + `Race_Ethnicity_NH Black` + Race_Ethnicity_Other, data = df_dummies)
summary(model3)

library(performance)
library(emmeans)


check_collinearity(model3)
check_heteroscedasticity(model3)
check_normality(model3)
check_outliers(model3)

# return a list of single plots
diagnostic_plots <- plot(check_model(model3, panel = FALSE))

diagnostic_plots[[2]]
diagnostic_plots[[3]]
diagnostic_plots[[4]]
diagnostic_plots[[5]]
diagnostic_plots[[6]]

emm <- emmeans(model3, pairwise ~ Race_Ethnicity_Hispanic)
emm
confint(emm, level = 0.95)

model3 <- lm(CIG_1TRI ~ Any_Abuse + ACEs_Scale + BPG_DEPRS8 + I(Race_Ethnicity), data = df_dummies)
summary(model3)
emm <- emmeans(model3, pairwise ~ Race_Ethnicity)
emm
confint(emm, level = 0.95)


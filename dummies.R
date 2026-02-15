# note to make sure the packages are installed

library(fastDummies)
library(haven)
library(dplyr)

admissions <- haven::read_sav("data/admissions.sav")
table(admissions$Race)

admissions_sub <- admissions %>%
  dplyr::select(row_number, Race) %>%
  filter(Race != "not_indicated")

table(admissions_sub$Race)


# ---------------------------------------------------
# Create dummy variables for race
# ---------------------------------------------------
admissions_wrong <- fastDummies::dummy_cols(
  admissions_sub,
  select_columns = "Race",
  remove_first_dummy = TRUE,        # creates k-1 dummies
  remove_selected_columns = FALSE    # if TRUE drops original race variable
)

# Inspect result
head(admissions_sub)
names(admissions_sub)

# What do you notice? The function removed the first variable which was Native American. This is how we get into trouble by not understanding what the code is doing or why.
# It is better to keep all variables and make sure you omit ONE in your regression, OR refactor the analysis to make sure the omitted variable
# contains enough cases. The obvious choice here is to remove "White" but note that this makes White the omitted category, or the reference group.
# You need to ask yourself whether you want "White" to be the locus of interpretation, and if not make sure your omitted group that you want
# the comparisons against is the one that is dropped. To make things easy I will NOT remove the first dummy.

admissions_sub <- fastDummies::dummy_cols(
  admissions_sub,
  select_columns = "Race",
  remove_first_dummy = FALSE,        # creates k-1 dummies
  remove_selected_columns = FALSE    # if TRUE drops original race variable
)
head(admissions_sub)
names(admissions_sub)

admissions <-  left_join(admissions, admissions_sub, by = "row_number")

# here are some regressions, what racial group is omitted?
summary(mod <- lm(SAT_math ~ SAT_verbal + HS_class_size + Race_Asian, admissions))
summary(mod <- lm(SAT_math ~ SAT_verbal + HS_class_size + Race_Asian + Race_Hispanic + Race_American_Indian + Race_white, admissions))
summary(mod <- lm(SAT_math ~ SAT_verbal + HS_class_size + Race_Asian + Race_Hispanic + Race_American_Indian + Race_white + Race_black, admissions))

admissions <- admissions %>% filter(Race_American_Indian != 1)
summary(mod <- lm(SAT_math ~ SAT_verbal + HS_class_size + Race_Asian + Race_Hispanic + Race_white, admissions))

# How would you compare Asian and White students?
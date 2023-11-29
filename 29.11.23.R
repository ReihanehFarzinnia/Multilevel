##### Week 9: Multilevel Modelling - Part 1 - Practical

# ---------------------------------------------------------------------------------------------

## Linear Mixed Models - teleprism data from pupils in classes in schools (3-level data)
## We'll only be dealing with 2 levels, pupils (level 1) nested in classes (level 2)


# We'll use the Linear Mixed Effects v4 package, lme4
#.libPaths(c("C:/R/library4.0.3/", .libPaths())) # set installation location
#install.packages("lme4")                        # install library
library(lme4)                                   # load library

# load some other libraries
library(tidyverse)
library(fastDummies)
library(haven)
library(psych)
install.packages("fastDummies")

# Set Working Directory
#setwd("C:/Users/mscssnms/Dropbox (The University of Manchester)/SOST new R courses/teleprism")

# Load the teleprism data, on pupil performance in classes and schools 
teleprism <- read_csv("teleprism.csv")

# Let's have a look at the data
View(teleprism)
describe(teleprism)
# 13524 pupils (_i) in 660 classes (_j) in 40 schools (_k)


###################################################################
#
#Research Question: Does the maths ability self-efficacy ("abilty_maths")
#of girls suffer when they are in classes with a greater preponderance of boys?
#
###################################################################

## How much variance in maths ability is between pupils within classes (e_ij), 
#vs. between classes (u_j)? This is a "variance components" model (i.e. no 
#predictors, we are just looking at how the variance in the outcome variable is 
#distributed across the levels: 
#
#  y_maths = b_00 + u_j + eij 

vcfit_maths <- teleprism %>%
  filter(!is.na(male))   %>%                    # filter out missing (NA)
  lmer(formula = ability_maths ~ (1|class_id)) #(Random effect only - without random intercept)
summary(vcfit_maths)

# 13,231 pupils (_ij) in 660 classes (_j) have non-missing sex and maths ability data. 
# Variance in y_maths between classes               u_j  = 0.1547 
# Variance in y_maths between pupils within classes e_ij = 0.4648
#
# Proportion of variance between classes u_j = 0.1547 / (0.1547 + 0.4684) = 0.2483
# Proportion of variance within classes e_ij = 0.4684 / (0.1547 + 0.4684) = 0.7517
#
# Interpretation: 
#  25% of the variation in maths ability is between classes.
#  75% of the variation in maths ability is between individuals within classes.


###############################################################################
#
#               Sex difference in maths ability
#
###############################################################################

# There are two levels at which sex difference might operate:
#  Level 1, i.e. Within classes: The difference between girls and boys
#    Variable = male (male = 1, female = 0)
#  Level 2, i.e. Between classes: The difference between classes with different proportions of 
#   girls/boys in the class
#    Variable = class_male_pr (proportion of males in a class)
#
# Level 1 effects can be called "compositional", i.e. they reflect what the level 2 units are composed of
#  (here, classes are composed of boys and girls).
# Level 2 effects can be called "contextual", i.e. they reflect the context for the level-1 units (here,
#   the sex-ratio of the class is the context for the individual pupils).
#
#  Note that "class_male_pr" is the same for all pupils in a class. 
#  More generally, variables that are measured at level 2 have no variation at level 1. 
#  Therefore, level 2 variables cannot account for variance at level 1. 

# Remind yourself of the amount of variance at levels 1 and 2
summary(vcfit_maths)

###############
#
# level 2 sex difference in maths ability - between classes
#
fit_sexb <- teleprism %>%
  filter(!is.na(male)) %>%                    # filter out missing (NA)
  lmer(formula = ability_maths ~ class_male_pr + (1|class_id))
summary(fit_sexb)

# The fixed effect of class_male_pr is 0.1721. 
# Interpretation: 
# The predicted difference in average maths ability between classes with all girls 
#  (class_male_pr = 0) and all boys (class_male_pr=1) is 0.17 out of 4. 
#
# Random effects:
# The level 1 residual variance e_ij is (within rounding error) unchanged at 0.4684
# The level 2 residual variance u_j has gone down from 0.1547 to 0.1527, 
#  which is a proportional reduction of (0.1547 - 0.1527) / 0.1547 = 0.0129, or 1.29%
# This can be interpreted as the level-2 R-square for class_male_pr
# 
# Interpretation
# 1.29% of the difference in average maths ability between classes can be attributed
#  to differences in the proportion of girls and boys in the classes.


###############
#
# Level 1 sex difference in maths ability - within classes
fit_sexw <- lmer(formula = ability_maths ~ male + (1|class_id), data = teleprism)
summary(fit_sexw)

# The fixed effect of male is 0.2147. 
# Interpretation: 
# The predicted difference in average maths ability is that boys score 0.21 points higher 
#  girls.  
#
# Random effects:
# The level 1 residual variance e_ij has gone down to 0.4596 from 0.4684, which is a proportional 
#  reduction of (0.4684 - 0.4596) / 0.4684 = 0.0188, or 1.88%
# The level 2 residual variance u_j has gone down from 0.1547 to 0.1529, 
#  which is a proportional reduction of (0.1547 - 0.1529) / 0.1547 = 0.0116, or 1.16%
# 
# Note that variables measured at level 1 can account for variance at both level 1 and level 2. 
#
# Interpretation
# The difference between boys and girls accounts for 1.9% of the variation in maths ability between 
#  pupils within classes, and 1.2% of the variation in average maths ability between classes. 


############### Combining both models, i.e. including compositional (level 1) and contextual (level 2)
#
# Sex difference in maths ability - between and within classes
fit_sexb_sexw <- teleprism %>%
  lmer(formula = ability_maths ~ male + class_male_pr + (1|class_id))
summary(fit_sexb_sexw)

# The fixed effect of male is 0.217, and of class_male_pr is -0.045.
# The effect of male is very large compared to its SE and has a low p-value.
# The effect of class_male_pr is very small compared to its SE and has a high p-value.
# The level-2 effect of sex adds almost nothing to the level-1 effect.
#
# Random effects:
# The level 1 residual variance e_ij is about the same as in the level-1 model, at 0.4595.
# The level 2 residual variance u_j  is about the same as in the level-1 model, at 0.1531. 
# Adding the level-2 effect of sex adds just about nothing to the variance accounted for by
#  the level-1 effect.
#
# Interpretation: 
# There is little evidence for between-class variation in maths ability due to sex ratio differences, 
#  after we account for the sex of individual pupils within the class.
#
# The effect of pupil sex on maths ability is compositional (i.e. the sexes the classes are composed of), 
#  not contextual (i.e. not the "context" of the class sex ratio).
#
# Answering the Research Question: 
#  There are between-class differences in average ability, such that classes with more males tend to
#   have slightly higher average ability. But these between-class differences are accounted for by
#   the fact that boys tend to have slighly higher ability than girls. There is is scant evidence here 
#   that girls in classes with more boys perform worse than girls in classes with more girls.

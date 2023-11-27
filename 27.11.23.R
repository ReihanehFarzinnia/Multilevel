##### Week 9: Multilevel Modelling

# ---------------------------------------------------------------------------------------------

library(tidyverse)
library(lattice)
library(moderndive)
library(skimr)
library(lme4)
library(foreign)

# ----------------------------------- Multilevel modelling -----------------------------------
# --------------------------------------------------------------------------------------------

## Loading data. 

mydata <- read.dta("school.dta") 
str(mydata)

## Fitting a simple linear regression (mydata$math ~ mydata$homework).
modOLS<-lm(mydata$math ~ mydata$homework)
summary(modOLS)

## visualise the fitted line using ggplot.
mydata %>%
  ggplot(aes(x = homework, y = math)) +
  geom_point() +
  geom_smooth(method = "lm", se= FALSE)

## visualise the fitted line using ggplot (fit one regression line to each of the 10 schools).
mydata %>%
  ggplot(aes(x = homework, y = math, colour = as.factor(schnum))) +
  geom_point() +
  geom_smooth(method = "lm", se= FALSE)

attach(mydata) # Attach the dataset.


# ------------------------>>> Random intercept model.

## fitting a random intercept model (math ~ homework + (1|schnum)).
modb0 <- lmer(math ~ homework + (1|schnum), REML = FALSE)
summary(modb0)

## plot (predmath vs homework).
predmath<-fitted(modb0)
xyplot(predmath ~ homework , mydata, groups=schnum, typ=c("p","l"), col="red")


# ------------------------>>> Random slope and intercept model.

## fitting random slope and intercept model (math ~ homework + (1 + homework|schnum)).
modb1<-lmer(math ~ homework + (1 + homework|schnum), REML = FALSE)
summary(modb1)

## Plot of the predicted school lines.
predmath<-fitted(modb1)
xyplot(predmath ~ homework , mydata, groups=schnum, type=c("p","l"), col="red")

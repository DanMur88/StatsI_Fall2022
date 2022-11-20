# Load libraries/packages
library(stargazer)
library(tidyverse)

# Import dataset
dat <- read.csv('~/GitHub/StatsI_Fall2022/datasets/incumbents_subset.csv')

###Question 1###

##Part 1##

# Run regression with outcome variable voteshare and explanatory variable difflog
mod1 <- lm(voteshare ~ difflog, data = dat)

# Output summary statistics of regression model as latex code
stargazer(mod1, type = 'latex', title = 'Summary Statistics of Regression Model')

##Part 2##

# Output scatterplot of voteshare and difflog with regression line
ggplot(dat, aes(x=difflog, y=voteshare)) + 
  geom_point(size=0.75) +
  geom_abline(intercept = mod1$coefficients[1], slope = mod1$coefficients[2], color = "red") +
  ggtitle("Scatterplot of voteshare ~ difflog") +
  theme(plot.title = element_text(hjust = 0.5))

##Part 3##

# Save residuals of the model in a separate object
mod1_res <- mod1$residuals

###Question 2###

##Part 1##

# Run regression with outcome variable presvote and explanatory variable difflog
mod2 <- lm(presvote ~ difflog, data = dat)

# Output summary statistics of regression model as latex code
stargazer(mod2, type = 'latex', title = 'Summary Statistics of Regression Model')

##Part 2##

# Output scatterplot of presvote and difflog with regression line
ggplot(dat, aes(x=difflog, y=presvote)) + 
  geom_point(size=0.75) +
  geom_abline(intercept = mod2$coefficients[1], slope = mod2$coefficients[2], color = "red") +
  ggtitle("Scatterplot of presvote ~ difflog") +
  theme(plot.title = element_text(hjust = 0.5))

##Part 3##

# Save residuals of the model in a separate object
mod2_res <- mod2$residuals

###Question 3###

##Part 1##

# Run regression with outcome variable voteshare and explanatory variable 
# presvote
mod3 <- lm(voteshare ~ presvote, data = dat)

# Output summary statistics of regression model as latex code
stargazer(mod3, type = 'latex', title = 'Summary Statistics of Regression Model')

##Part 2##

# Output scatterplot of voteshare and presvote with regression line
ggplot(dat, aes(x=presvote, y=voteshare)) + 
  geom_point(size=0.75) +
  geom_abline(intercept = mod3$coefficients[1], slope = mod3$coefficients[2], color = "red") +
  ggtitle("Scatterplot of voteshare ~ presvote") +
  theme(plot.title = element_text(hjust = 0.5))

###Question 4###

##Part 1##

# Run regression with outcome variable mod1_res and explanatory variable 
# mod2_res
mod4 <- lm(mod1_res ~ mod2_res)

# Output summary statistics of regression model as latex code
stargazer(mod4, type = 'latex', title = 'Summary Statistics of Regression Model')

##Part 2##

# Output scatterplot of mod1_res and mod2_res with regression line
ggplot(dat, aes(x=mod2_res, y=mod1_res)) + 
  geom_point(size=0.75) +
  geom_abline(intercept = mod4$coefficients[1], slope = mod4$coefficients[2], color = "red") +
  ggtitle("Scatterplot of mod1_res ~ mod2_res ") +
  theme(plot.title = element_text(hjust = 0.5))

###Question 5###

##Part 1##

# Run regression with outcome variable voteshare and explanatory variables mod2_res
mod5 <- lm(voteshare ~ difflog + presvote, data = dat)

# Output summary statistics of regression model as latex code
stargazer(mod5, type = 'latex', title = 'Summary Statistics of Regression Model')

#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

install.packages('tidyverse')
install.packages('corrplot')
install.packages("Hmisc")

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("tidyverse"),  pkgTest)

library(tidyverse)
library("corrplot")
library("Hmisc")
library("ggplot2")

# set working directory
setwd("~/Documents/GitHub/QTM200Spring2021/problem_sets/PS1")


#####################
# Problem 1
#####################

### Part 1 ###

# Create an object for IQ of the random sample of 25 students.

IQ <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 
        113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Plot distribution of the sample data

plot(density(IQ), main = "Density Plot of Student IQ Sample")

# Calculate number of observations and mean 

n_IQ <- length(IQ)

mean_IQ <- sum(IQ)/length(IQ)

# Calculate standard deviation and save as object

sq_diff <- vector("double", length = length(IQ))

for (i in seq_len(length(IQ))) {
  sq_diff[i] <- (IQ[i]-mean_IQ)^2
}

sd_IQ <- sqrt((sum(sq_diff))/(n_IQ - 1))

#Calculate standard error and save as object
se_IQ <- sd_IQ / sqrt(n_IQ)

# Construct a 90% confidence interval for the average IQ of the 
# population of all students in the school using a t-distribution, as
# the sample size n is relatively small and the exact standard error
# of the population is unknown. We need to substitute the sample standard deviation "s" 
# for the population standard deviation to get the estimated standard
# error, which introduces extra error and necessitates replacing the
# z-score with a t-score.
#
# Begin by finding the relevant t-score for a 90% confidence 
# interval with df = 24 (n-1), making the assumption that the 
# sampling distribution of the sample mean is approximately normal. 
# Input this value as an object.

tscore_IQ <- qt(0.95, 24, lower.tail = TRUE)

# Calculate the margin of error and record it as an object.

MOE <- tscore_IQ * se_IQ

# Construct our 90% confidence interval by subtracting the 
# margin of error from our point estimator (sample mean) to 
# establish its lower bound, and adding the margin of error to our
# point estimator (sample mean) to establish its upper bound.

CI_lower <- mean_IQ - MOE
CI_upper <- mean_IQ + MOE

### Part 2 ###

# Test for the null hypothesis that mean_IQ = 100, with the
# alternative hypothesis being mean_IQ > 100.

# Calculate the value of the test statistic.

tscore_null <- ((mean_IQ - 100))/se_IQ

# As we are testing the probability that the average IQ of students 
# in our sample is higher than the average IQ score, i.e. P[X≤x], 
# we find the right-tailed P-score for our test statistic, the 
# t-score -0.59574, with degrees of freedom df = 24.

p_value <- pt(abs(tscore_null), n_IQ-1, lower.tail = T)

# The right-tailed P-value is 0.72. Therefore we do not reject the null 
# hypothesis at a 0.05 significance level.

#####################
# Problem 2
#####################

# Import "expenditure" dataset into R.

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2022/main/datasets/expenditure.txt", header=T)

### Part 1 ###

# Use pairs() function to construct multiple scatter plots to show 
# the relationships among each pair of the numeric variables Y, X1, 
# X2 and X3.

pairs(expenditure[, c("Y", "X1", "X2", "X3")], 
      main = "Relationships among Y, X1, X2, X3")

# We can also visualise these relationships on a correlogram.

# Use rcorr() function to return the correlation coefficients and 
# P-values for all possible pairs of the variables.

exp_rcorr <- rcorr(as.matrix(expenditure[c(2, 3, 4, 5)]))

# Subset the correlation matrix r, rounded to two decimal points.

M <- round(exp_rcorr$r,2)

# Subset the matrix of p-values P.

p_mat <- exp_rcorr$P

# Use corrplot() function to plot correlation matrix as correlogram, 
# marked for insignificant coefficients at a 0.05 significance level.
# Ordered with hierarchical clustering of correlation coefficients.

corrplot(M, type = "lower", order = "hclust", addCoef.col = "white", 
         p.mat = p_mat, sig.level = 0.05, tl.srt = 0, tl.offset = 1)

# Ref: https://rstudio-pubs-static.s3.amazonaws.com/240657_5157ff98e8204c358b2118fa69162e18.html

### Part 2 ###

# Convert "Region" variable into a factor

expenditure$Region <- as.factor(expenditure$Region)

# Rename levels to name of regions
levels(expenditure$Region) <-c("Northeast", "North Central", 
                               "South", "West")

# Plot the relationship between Y and Region on boxplots 

boxplot <- ggplot(expenditure, aes(x=Region, y=Y)) +
  geom_boxplot() +
  theme( legend.position = "none" ) + 
  stat_summary(fun="mean", color="red", shape=4, size=0.1) +
  ggtitle("Relationship between State Housing \nAssistance Expenditure and Region") +
  labs(x="Region", 
       y="Per capita expenditure on \nshelters/housing assistance") +
  theme(plot.title = element_text(hjust = 0.5))

boxplot

### Part 3 ###

# Plot the relationship between Y and X1 on a scatter plot
scatterplot <- ggplot(expenditure, aes(x=Y, y=X1)) +
  geom_point() +
  ggtitle("Relationship between State Housing \nAssistance Expenditure and Personal Income") +
  labs(x="Per capita expenditure on housing assistance in state", 
       y="Per capita personal income in state") +
  theme(plot.title = element_text(hjust = 0.5))

scatterplot

# Reproduce scatter plot to include Region variable, displayed as different
# symbols and colours

scatterplot2 <- ggplot(expenditure, aes(x=Y, y=X1, shape=Region, 
                                       color=Region)) +
  geom_point() +
  ggtitle("Relationship between State Housing \nAssistance Expenditure and Personal Income") +
  labs(x="Per capita expenditure on housing assistance in state", 
       y="Per capita personal income in state") +
  theme(plot.title = element_text(hjust = 0.5))

scatterplot2

     
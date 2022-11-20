#####################
# Question 1
#####################

### Part (a) ###

# Input observed frequencies
o_uppernotstopped <- 14
o_upperbribe <- 6
o_upperstopped <- 7
o_lowernotstopped <- 7
o_lowerbribe <- 7
o_lowerstopped <- 1

# Calculate row totals for the explanatory variable (class)
total_upper <- sum(o_uppernotstopped, o_upperbribe, o_upperstopped)
total_lower <- sum(o_lowernotstopped, o_lowerbribe, o_lowerstopped)

# Calculate column totals for the response variable (bribe outcome)
total_notstopped <- sum(o_uppernotstopped, o_lowernotstopped)
total_bribe <- sum(o_upperbribe, o_lowerbribe)
total_stopped <- sum(o_upperstopped, o_lowerstopped)

# Calculate overall sample size
total_sample <- sum(total_upper, total_lower)

# Calculate expected frequencies that would satisfy a null hypothesis 
# of independence
e_uppernotstopped <- (total_upper*total_notstopped/total_sample)
e_upperbribe <- (total_upper*total_bribe/total_sample)
e_upperstopped <- (total_upper*total_stopped/total_sample)
e_lowernotstopped <- (total_lower*total_notstopped/total_sample)
e_lowerbribe <- (total_lower*total_bribe/total_sample)
e_lowerstopped <- (total_lower*total_stopped/total_sample)

# Calculate chi-squared test statistic by 1) for each cell, squaring the 
# differences between the observed and expected frequencies and then dividing 
# that square by the expected frequency, and 2) summing these values.
chi_sqrd <- 
  (((o_uppernotstopped - e_uppernotstopped)^2)/e_uppernotstopped) + 
  (((o_upperbribe - e_upperbribe)^2)/e_upperbribe) + 
  (((o_upperstopped - e_upperstopped)^2)/e_upperstopped) +
  (((o_lowernotstopped - e_lowernotstopped)^2)/e_lowernotstopped) +
  (((o_lowerbribe - e_lowerbribe)^2)/e_lowerbribe) +
  (((o_lowerstopped - e_lowerstopped)^2)/e_lowerstopped)

chi_sqrd

### Part (b) ###

# Calculate degrees of freedom (df)
df <- (2-1)*(3-1)
df

# Calculate P-value for the test statistic
p_value <- pchisq(chi_sqrd, df, lower.tail = FALSE)
p_value

### Part (c) ###

# Calculate the standardised residuals for each cell
sr_uppernotstopped <- (o_uppernotstopped - e_uppernotstopped) / 
  sqrt(e_uppernotstopped*(1-(total_upper/total_sample))*(1-(total_notstopped/total_sample)))
sr_upperbribe <- (o_upperbribe - e_upperbribe) / 
  sqrt(e_upperbribe*(1-(total_upper/total_sample))*(1-(total_bribe/total_sample)))
sr_upperstopped <- (o_upperstopped - e_upperstopped) / 
  sqrt(e_upperstopped*(1-(total_upper/total_sample))*(1-(total_stopped/total_sample)))
sr_lowernotstopped <- (o_lowernotstopped - e_lowernotstopped) / 
  sqrt(e_lowernotstopped*(1-(total_lower/total_sample))*(1-(total_notstopped/total_sample)))
sr_lowerbribe <- (o_lowerbribe - e_lowerbribe) / 
  sqrt(e_lowerbribe*(1-(total_lower/total_sample))*(1-(total_bribe/total_sample)))
sr_lowerstopped <- (o_lowerstopped - e_lowerstopped) / 
  sqrt(e_lowerstopped*(1-(total_lower/total_sample))*(1-(total_stopped/total_sample)))


#####################
# Question 2
#####################

### Part (b) ###

# Import data file
data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv", header=T)

# Create a subsetted data frame to remove GPs not reserved for female leaders but which have a female leader
data2 <- rbind(data[data$reserved == "1",], data[data$female == "0",])

# Run bivariate linear regression with "water" as response variable and "female" as the explanatory variable
ols <- lm(water ~ female, data = data2)
summary(ols)

# Calculate alpha and beta coefficients by hand
beta <- sum((data2$water - mean(data2$water))*
              (data2$female - mean(data2$female)))/
  sum((data2$female - mean(data2$female))^2)

alpha <- mean(data2$water) - beta*mean(data2$female) 

### Part (c) ###

# Calculate residual standard error
rse <- sqrt(sum(ols$residuals^2)/(length(data2$water)-2))
rse

# Calculate total sum of squares of x (female)
TSSx <- sum((data2$female-mean(data2$female))^2)
TSSx

# Calculate standard error for beta
SEb <- rse/(sqrt(TSSx))
SEb

# Construct 95% confidence interval for our coefficient estimate for reservation
# policy
CI_high <- ols$coefficients[2]+1.9678*SEb
CI_low <- ols$coefficients[2]-1.9678*SEb

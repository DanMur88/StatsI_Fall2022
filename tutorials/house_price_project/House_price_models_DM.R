library("tidyverse")
library(broom)
library(stargazer)
install.packages("stargazer")
library('stargazer')

dat <- train
dat <- readRDS("data/train.rds")
dat2 <- c(dat$SalePrice, dat$zhvi_px, dat$zhvi_idx, dat$AdjSalePrice,
          dat$NbrLivingUnits, dat$SqFtLot, dat$SqFtTotLiving, dat$SqFtFinBasement,
          dat$Bathrooms, dat$Bedrooms, dat$BldgGrade, dat$LandVal)

dat2 <- data.frame(dat)

mod1 <- lm( AdjSalePrice ~  SqFtTotLiving + LandVal + NbrLivingUnits + 
              Bathrooms + BldgGrade + TrafficNoise, data = dat)
dat_add <- augment(mod1)
stargazer(mod1, type = "text", title = "a")

mod2 <- lm( AdjSalePrice ~  SqFtTotLiving + LandVal + 
              Bathrooms + BldgGrade + TrafficNoise, data = dat)
stargazer(mod1, mod2, type = "text", title = "b")

mod3 <- lm( AdjSalePrice ~  SqFtTotLiving + LandVal + NbrLivingUnits + 
              Bathrooms + BldgGrade + TrafficNoise + SqFtFinBasement + SqFtLot
            + Bedrooms + ImpsVal, data = dat)
stargazer(mod1, mod3, type = "text", title = "c")

mod4 <- lm( AdjSalePrice ~  SqFtTotLiving + LandVal + NbrLivingUnits + 
             + BldgGrade + SqFtFinBasement + SqFtLot
            + Bedrooms + ImpsVal, data = dat)
dat_add <- augment(mod4)
stargazer(mod1, mod4, type = "text", title = "c")

mod5 <- lm( AdjSalePrice ~  SqFtTotLiving + LandVal + NbrLivingUnits + 
              + BldgGrade + SqFtFinBasement + SqFtLot + zhvi_px
            + ImpsVal + Bedrooms + YrBuilt, data = dat)

stargazer(mod4, mod5, type = "text", title = "c")


summary(mod3)
summary(mod5)
summary(mod4)

mod6 <- lm( AdjSalePrice ~  SqFtTotLiving + LandVal + 
            ImpsVal, data = dat)

stargazer(mod6, type = "text", title = "c")
summary(mod6)
PRESS(mod6)
pred_r_squared(mod6)
pred_r_squared(mod1)
summary(mod6)$r.squared

#PRESS - predicted residual sums of squares

PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)

  return(PRESS)
}


pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  
  return(pred.r.squared)
}

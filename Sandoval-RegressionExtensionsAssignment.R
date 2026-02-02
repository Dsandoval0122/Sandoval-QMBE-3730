## Title: Regression Extensions Assignment 
## Author: Dominic Sandoval 
## Date: February 1, 2026 

## Load Packages
library(tidyverse)
library(ggplot2)
library(lmtest)
library(MASS)
library(car)

# 1. 
# Load Data
getwd()
setwd("/Users/dominics./downloads")

wages <- read_xlsx("wages.xlsx") 

# View and summarize Data 
view(wages)
summary (wages)

## Clean data 
sum(is.na(wages))
 
# Data has already been cleaned, no further action needed.

## a.Plot Wage against Age and evaluate whether a linear or quadratic model would better capture the relationship. 

ggplot(wages, aes(x = Age, y = Wage)) +
  geom_point(alpha = 0.5) +
  ggtitle("Scatterplot: Age vs Wages") +
  xlab("Age") +
  ylab("Hourly Wages($)") +
  theme_minimal()

## From this graph, the relationship between Age and wages 
## would be better displayed in a quadratic model compared to a linear model. 
## This is because the data points demonstrate a curved pattern. The wage amounts (x) seem to increase until about 60, 
## then show a gradual decrease as age increase after around the 60 years of age mark. 



## b.Estimate a multiple regression model of Wage using Age and Education as 
## independent (X) variables; assume a standard linear relationship between Wage and Age.

model_multi <- lm(Wage ~ ., data = wages)
summary(model_multi)

## From this regression, the y-intercept means that the hourly wage is 
## $2.64 dollars when both dependent values= 0. (Note that this isn't completely evident considering people are not starting to work at zero years old.)
## Between the two variables, Education is the only dependent variable that is statistically significant when p=.05. 


## c.Estimate another multiple regression model of Wage using Age and Education 
## as independent (X) variables; this time fit Age using a quadratic relationship. 
## Verify your choice from part a. by comparing the distribution of residuals and the goodness of fit between the models in parts b and c.

# Quadratic Regression
model_quad <- lm(Wage ~ Age + I(Age^2) + Educ, data = wages)
summary(model_quad)

# Residual Analysis of Residual Models (Multiple Linear Model)
par(mfrow = c(1, 2))
plot(model_multi$fitted.values, model_multi$residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

hist(model_multi$residuals, main = "Histogram of Residuals",
     xlab = "Residuals", breaks = 30) 

# Residuals are normal, assumption satisfied. 

# Residual Analysis of Residual Models (Quadratic Model)
par(mfrow = c(1, 2))
plot(model_quad$fitted.values, model_quad$residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

hist(model_quad$residuals, main = "Histogram of Residuals",
     xlab = "Residuals", breaks = 30)

# Residuals are normal, assumption satisfied. 

## Use the appropriate model to predict hourly wages for someone with 16 years of education and age equal 30, 50, or 70.
# Create a new data frame for prediction
new_data <- data.frame(Age = 30 ,Educ = 16) 

# Predict the value
predict(model_quad, newdata = new_data)


predict(model_quad, newdata = new_data)

## For someone who is 30 with 16 years of education, their expected hourly wage is 
# $25.85/hr. 

## d.According to the model, at what age will someone with 16 years of education attain the highest wages?


coeffs <- coef(model_quad)
beta_age <- coeffs["Age"]
beta_age2 <- coeffs["I(Age^2)"]
peak_age <- -beta_age / (2 * beta_age2)
print(paste("Wages peak at age:", round(peak_age, 1)))

## For someone who has 16 years of education, there peak wage 
## will occur at 50.7 years old. 

 ## 2.
# Load Data

getwd()
setwd("/Users/dominics./downloads")

AA <- read_xlsx("AnnArbor.xlsx") 

## Summary of data

summary(AA)
sum(is.na(wages))

# Data is clean, No further action needed. 

# a.Plot Rent against each of the three predictor variables and evaluate whether the relationship is best captured by a line or a curve. Identify variables that may benefit from a log-transformation.

## Rent Vs. # of Beds

ggplot(AA, aes(x = Beds, y = Rent)) +
  geom_point(alpha = 0.5) +
  ggtitle("Scatterplot: Number of Beds vs Rent") +
  xlab("Number of Beds") +
  ylab("Rent") +
  theme_minimal()

## Rent Vs. # of Baths

ggplot(AA, aes(x = Baths, y = Rent)) +
  geom_point(alpha = 0.5) +
  ggtitle("Scatterplot: Number of Baths vs. Rent") +
  xlab("Number of Baths") +
  ylab("Rent") +
  theme_minimal()

## Rent Vs. Square Feet

ggplot(AA, aes(x = Rent, y = Sqft)) +
  geom_point(alpha = 0.5) +
  ggtitle("Scatterplot: Square Feet vs Rent") +
  xlab("Square Feet") +
  ylab("Rent") +
  theme_minimal()

## From these different graphs, it is evident that the relationship
## between Square Feet and Rent. From this distribution of data, a log transformation would
## help stabilize some of the variance in this model. 

##b.Estimate a multiple regression model 
## (with any appropriate log-transformations) 
## to predict rent for a 1,600-square-foot rental with 3 bedrooms and 2 bathrooms.

## Create Model

log_model <- lm(log(Rent) ~ log(Sqft)+ Beds + Baths, data = AA)

summary(log_model)

## Prediction of model

new_rental <- data.frame(Sqft = 1600, Beds = 3, Baths = 2)

## Prediciton in Log Dollars 
log_prediction <- predict(log_model, newdata = new_rental)

## Prediction of Actual Rent 
predicted_rent <- exp(log_prediction)
print(paste("Estimated Rent (Log-Log Model): $", round(predicted_rent, 2)))

# From this model, the predicted rent would be $1,486.24. 




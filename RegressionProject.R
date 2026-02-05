# Title: Regression Project
# Authors: Dominic Sandoval and Allen Walker 
# Date: February 8 2026 

# Load Packages 
library(tidyverse)
library(ggplot2)
library(lmtest)
library(MASS)
library(car)

# Load Data and Set Working Directory
getwd()
setwd("/Users/dominics./downloads")

College <- read_xlsx("CollegeTowns.xlsx") 

## View Data set and clean data 
summary(College)
sum(is.na(College))

# Data set is cleaned, Duplicates are irrelevant considering each category is continuous data (No duplicate cities).
# Outliers are present, but we chose acknowledge and keep them as they are not the result of data input error. 

## Scatter plots between Rent and each independent variable. 

# Rent Vs. Population

ggplot(College, aes(x =pop, y = rent)) +
  geom_point(alpha = 0.5, color="darkblue") +
  geom_smooth(method = "lm", se = TRUE, color="darkblue") 
  ggtitle("Scatterplot: Population of Town vs Rent Price") +
  xlab("Population") +
  ylab("Rent Price ($)") +
  theme_minimal()

# Rent Vs. Average Income 
ggplot(College, aes(x =avginc, y = rent)) +
  geom_point(alpha = 0.7, color="cyan") +
  geom_smooth(method = "lm", se = TRUE, color="cyan") 
  ggtitle("Scatterplot: Average Income vs Rent Price") +
  xlab("Rent Occupied Units") +
  ylab("Rent Price ($)") +
  theme_minimal()

# Rent Vs. College Enrollment

ggplot(College, aes(x =enroll, y = rent)) +
  geom_point(alpha = 0.5, color="red") +
  geom_smooth(method = "lm", se = TRUE, color="red") +
  ggtitle("Scatterplot: College Enrollment vs Rent Price") +
  xlab("College Enrollment") +
  ylab("Rent Price ($)") +
  theme_minimal()

# Rent Vs. Rent Occupied Units 

ggplot(College, aes(x =rnthsg, y = rent)) +
  geom_point(alpha = 0.5, color="orange") +
  geom_smooth(method = "lm", se = TRUE, color="orange") +
  ggtitle("Scatterplot: Rent Occupied Units vs Rent Price") +
  xlab("Rent Occupied Units") +
  ylab("Rent Price ($)") +
  theme_minimal()

# Rent Vs. Occupied Housing Units 
ggplot(College, aes(x =tothsg, y = rent)) +
  geom_point(alpha = 0.5, color="green") +
  geom_smooth(method = "lm", se = TRUE, color="green") 
  ggtitle("Scatterplot: Occupied Housing Units vs Rent Price") +
  xlab("Occupied Housing Units") +
  ylab("Rent Price ($)") +
  theme_minimal()

# Rent Vs. College Population %

ggplot(College, aes(x =pctstu, y = rent)) +
  geom_point(alpha = 0.5, color="blue") +
  geom_smooth(method = "lm", se = TRUE) +
  ggtitle("Scatterplot: Percentage of College Students vs Rent Price") +
  xlab("Percentage of College Students") +
  ylab("Rent Price ($)") +
  theme_minimal()

# Corrplot between variables 

correlation_matrix <- cor(College[, 1:8], method = "pearson", use = "pairwise.complete.obs")
print(correlation_matrix)

heatmap(correlation_matrix)

# From these observations we can predict that the average income is the best predictor for
# predicting the rent price in college towns. 

## Overall, these scatter plots highlight linear relationship between the independent variables and 
# rent price. 

# Base Multiple Regression Model for College Data set 

model_multi <- lm(rent ~ pop + rnthsg + avginc + tothsg + pctstu + enroll, data = College)
summary(model_multi)

## Important Notes: 
# Holding everything else constant, the rent price is $107.5 when every independent variable is zero. 
# Population, occupied rent units, and average income are the only statistically significant variables. 
# Multiple R Squared= 65%, meaning that only 65% of the variance can be explained by this model.

## Residuals of Base Model 

par(mfrow = c(1, 2))
plot(model_multi$fitted.values, model_multi$residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

hist(model_multi$residuals, main = "Histogram of Residuals",
     xlab = "Residuals", breaks = 30)

## Base model demonstrates that the residuals are right skewed, under predicting for some observations due to statsitically insignifcant variables.  

## Multiple Regression Model with removal of insignificant variables. 

model_multi_2 <- lm(rent ~ pop + rnthsg + avginc, data = College)
summary(model_multi_2)

# Residuals for Model 2 
## Residuals of Base Model 

par(mfrow = c(1, 2))
plot(model_multi_2$fitted.values, model_multi_2$residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

hist(model_multi_2$residuals, main = "Histogram of Residuals",
     xlab = "Residuals", breaks = 30)

# Due to the right skewness of the second model, a log regression model may be optimal for this data set. 

## Log Regression Model 
log_model <- lm(log(rent) ~ log(avginc) + pop + rnthsg + avginc, data = College)
summary(log_model)

## Residual Analysis for Log Model

par(mfrow = c(1, 2))
plot(log_model$fitted.values, log_model$residuals, 
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

hist(log_model$residuals, main = "Histogram of Residuals",
     xlab = "Residuals", breaks = 30)

# While still not perfect, the residuals are more evenly distributed that the two previous models. 

## Prediction Example using Logistic Regression 

# For this example, we are going to predict the rent price in a town that has an average income of $3500, a population of 25,000, and 14,000 rent occupied units.  

new_town <- data.frame(avginc = 3500, pop = 25000, rnthsg = 14000)

# Prediction in log dollars
log_prediction <- predict(log_model, newdata = new_town)

## Prediction of Actual Rent 
predicted_rent <- exp(log_prediction)
print(paste("Estimated Rent (Log-Log Model): $", round(predicted_rent, 2)))

## For a college town that has these data points, the predicted rent is expected to $646.44


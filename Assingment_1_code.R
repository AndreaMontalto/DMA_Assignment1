### Assignment 1 code ####
library(writexl)
library(readxl)
library(car)
library(lmtest)
library(sandwich)
#Part I: Working on the training data set
trainingset <- read.csv('training data set.csv')

#y is the outcome variable 

## Question 1: Compute meand of y ## 

mean_y <- mean(trainingset$y)

## Question 2: Which IV has the highest correlation to the DV ## 

cor(trainingset) #variable x7 shows the highest correlation of 0.921

## Question 3: Building a linear model ## 

model_lm <- lm(y ~., data = trainingset)
summary(model_lm) #The model shows as adjusted R-squared of 0.937

## Question 4: Report p-value corresponding to x4

#the p-value of x4 is: 0.00404

## Question 5: Using a significance level of 2.5%, infer if the relationship between x4 and y is significant ## 

#as the p-value of x4 is 0.404%, we infer that there is a signifact relationship

## Question 6: 

pairs(trainingset, col = 'orange', lower.panel=panel.smooth)
#variables x3 and x5 appear not to be significant so, even though they are part of the true relationship, these can be dropped 

## Question 7: Check the noise in the model ##
qqnorm(model_lm$residuals)
qqline(model_lm$residuals)

hist(model_lm$residuals, breaks = 25, xlab = "Residuals", ylab = "Frequency",
     main = "Histogram of Residuals", col = "white", prob = TRUE)


#checking for heteroskedasticity
bptest(model_lm) #p-value above 0.05 so there is heteroskedasticity

plot(model_lm$residuals)


#indeed, by plotting a Q-Q plot and an histogram it seems that the assumption of normality is not broken

## PART II: Using test dataset ## 

testset <- read.csv('testing data set-7.csv')

## Question 8: Report the smallest p-value on the model run on the test set ## 

model_test <- lm(y ~., data = testset)
summary(model_test) #the smallest p-value is the one of x4: 0.00337

#checking for linearity 
plot(model_test$residuals)
abline(0,0, color= 'red')

## Question 9: Examine the presence of multicollinearity ## 
# We can check for multicollinearity by computing the Variance inflation factor (VIF)

vif(model_test) #there is multicollinearity among variables x2 and x3

## Question 10: Handle multicollinearity ## 

model_test_good <- lm(y ~ x1 + x3 + x4 + x5, data = testset)
summary(model_test_good) #p-value really small: 3.021e-12
plot(model_test$residuals)

## Question 11: 
plot(model_test_good$residuals)

#Question 12A: Identify the interaction factor 
summary(model_test_good)
model_int <- lm(y ~ x1 + x3 + x4 + x5 + x1:x3 + x1:x4 + x1:x5 + x3:x4 + x3:x5 + x4:x5, data = testset)
summary(model_int)

model_int_1 <- lm(y ~ x1 + x3 + x4 + x5 + x1:x5, data = testset) #the model with the interaction x1-x5 has a highest R-squared
summary(model_int_1)
model_int_2 <- lm(y ~ x1 + x3 + x4 + x5 + x4:x5, data = testset)
summary(model_int_2)

## Question 13: 
model_Q13 <- lm(y ~ x3 + x4 + x1:x5, data = testset)
summary(model_Q13)

hist(model_Q13$residual, breaks = 25, xlab = "Residuals", ylab = "Frequency",
     main = "Histogram of Residuals", col = "white", prob = TRUE)
plot(model_Q13$residuals, testset$x1)
plot(model_Q13$residuals, testset$x2)
plot(model_Q13$residuals, testset$x5)

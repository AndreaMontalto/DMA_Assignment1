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

#as the p-value of x4 is 0.404%, we infer that there is a significant relationship

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
abline(0, 0, col = "red")

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

vif(model_test) #there is multicollinearity among variables x1 and x2

## Question 10: Handle multicollinearity ## 

model_test_good <- lm(y ~  x3 + x4 + x5, data = testset)
summary(model_test_good) #p-value really small: 0.007247
plot(model_test$residuals)

## Question 11: 
plot(model_test_good$residuals)

#Question 12A: Identify the interaction factor 
summary(model_test_good)
model_int <- lm(y ~ x2+ x3 + x4 + x5 + x3:x4 + x2:x3 +x2:x4+ x2:x5 + x3:x4 +x3:x5 + x4:x5, data = testset)
summary(model_int)
# the most significant interaction is x3:x4

## Question 13: 
model_Q13 <- lm(y ~ x2 + x5 + x3:x4, data = testset)
summary(model_Q13)

hist(model_Q13$residual, breaks = 25, xlab = "Residuals", ylab = "Frequency",
     main = "Histogram of Residuals", col = "white", prob = TRUE)
plot(model_Q13$residuals, testset$x2)
abline(0,0, color= 'red')
plot(model_Q13$residuals, testset$x5)
abline(0.4,0, color= 'red')

## Question 14: plotting residuals against predictors that were not used ## 
# x1, x3, x4 
hist(model_Q13$residuals, breaks = 25, xlab = "Residuals", ylab = "Frequency",
     main = "Histogram of Residuals", col = "white", prob = TRUE)
plot(model_Q13$residuals, testset$x1)
plot(model_Q13$residuals, testset$x3)
plot(model_Q13$residuals, testset$x4)

qqnorm(model_Q13$residuals)
qqline(model_Q13$residuals) #normality holds 

vif(model_Q13) #multicollinearity holds

plot(testset$x5, model_Q13$residual, xlab="X", ylab="Residuals",main = "Plot residuals against X") 
abline(0,0, color= 'red') #x5 does not seem linear
     
## Question 15: Solving normality ## 
testset$x5_squared <- testset$x5 * testset$x5
model_Q15 <- lm(y ~x2 + x5 + x5_squared + x3:x4, data = testset)
summary(model_Q15)

#we can remove x5 as not significant 
model_Q15 <- lm(y ~x2 + x5_squared + x3:x4, data = testset)
summary(model_Q15)

qqnorm(model_Q15$residuals)
qqline(model_Q15$residuals) # normality holds

plot(testset$x2, model_Q15$residual, xlab="X2", ylab="Residuals",
     main = "Plot residuals against X2")

plot(testset$x5_squared, model_Q15$residual, xlab="X5", ylab="Residuals",
     main = "Plot residuals against X5_squared")
## Q16:Checking for heteroskedasticity ## 
bptest(model_Q15)
plot(fitted(model_Q15), residuals(model_Q15), main = "Residuals vs. Fitted")

#there is heteroskedasticity

#Q18: Checking for exogenity ## 
model_Q18 <- lm(y ~ x2 + x3:x4 , data = testset)
summary(model_Q18)

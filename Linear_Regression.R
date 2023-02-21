
### LAB

library(ISLR2)

data("Boston")

head(Boston)
?Boston

# simple linear regression
lm.fit <- lm(medv ~ lstat, data = Boston)

# alternatively, we could have written attach(Boston) and then called lm(medv ~ lstat) without specifying data = Boston

summary(lm.fit) # overall summary of the model
names(lm.fit) # what information is stored in lm.fit
coef(lm.fit) # returns the coefficients of the model
confint(lm.fit) # obtaining confidence intervals for coefficients

predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "confidence") # predicting new values with 95% confidence interval
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "prediction") # predicting new values with prediction intervals
### prediction interval is always wider than confidence interval, because it also takes into account the error term in addition to coefficient estimates

plot(Boston$lstat, Boston$medv, col = "blue", pch = "+")
abline(lm.fit, lwd = 3, col = "red")
# we can observe non-linearity between lstat and medv

### abline function can be used to draw any line, not just fitted line
### for example abline(a,b) will produce plot with a intercept and b slope
### lwd is the width of the line
### we can also use pch to create different symbols
### plot (1:20 , 1:20, pch = 1:20)

### We can use par() and mfrow() functions to view multiple plots simultaneously

par(mfrow = c(2, 2)) # divides the plotting region to 2x2 grid
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit)) # residuals
plot(predict(lm.fit), rstudent(lm.fit)) # studentized residuals
plot(hatvalues(lm.fit)) # leverage statistics
which.max(hatvalues(lm.fit)) # returns the index of the largest leverage statistics

### Multiple Linear Regression
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

lm.fit <- lm(medv ~ ., data = Boston) # fitting all of the data
summary(lm.fit)
summary(lm.fit)$r.sq # directly getting the r squared of the regression
summary(lm.fit)$sigma # get the RSE of the regression

library(car) # required to load to run vif()
vif(lm.fit) # calculating Variance Inflation factor for multicolliniarity

lm.fit1 <- lm(medv ~ . -age, data = Boston) # running regression on all but age, which has high p-value
summary(lm.fit1)
### Alternatively update() function can be used
lm.fit1 <- update(lm.fit, ~ . -age)

### Interaction terms
### we can use lstat:black to include interaction term, or lstat*age to include lstat, age and their interaction term lstat:age
summary(lm(medv ~ lstat*age, data = Boston))

### Polynomial Regression
lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(lm.fit2)
### using anove to futher quantify the extend to which quandratic fit is superior to linear fit
lm.fit1 <- lm(medv ~ lstat, data = Boston)
anova(lm.fit1, lm.fit2) # performs hypothesis test. The null hypothesis is that two models fit the data equally well
### The alternative hypothesis is that full model is superior
### The F-statistic is 135 and p-value is very close to zero, so quadratic fit is far superior to linear fit
par(mfrow = c(2, 2))
plot(lm.fit2)

### In order to fit cubic function we could still use I(lstat^3), but there is a faster alternative
lm.fit5 <- lm(medv ~ poly(lstat, 5), data = Boston) # polynomial function up to 5th power
summary(lm.fit5)

summary(lm(medv ~ log(rm), data = Boston)) # using log transformation for independent variable rm

### Qualitative Predictors
library(ISLR2)
data("Carseats")
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)
contrasts(Carseats$ShelveLoc) # coding that R used for dummy variable




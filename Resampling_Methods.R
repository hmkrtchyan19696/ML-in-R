### Cross Validation
options(warnings(-1))
library(ISLR2)

set.seed(1)
train <- sample(392, 196) # selecting a random subset of the original observations (392)

lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

attach(Auto)
test.MSE1 <- mean((mpg - predict(lm.fit, Auto))[-train]^2) # calculating mean test MSE for our regression

# Quadratic and cubic regression test error rates

lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
test.MSE2 <- mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
test.MSE3 <- mean((mpg - predict(lm.fit3, Auto))[-train]^2)

### Leave-one-out Cross-Validation LOOCV
### LOOCV estimate can be automatically computed for any generalized linear models using glm() and cv.glm() functions

glm.fit <- glm(mpg ~ horsepower, data = Auto, subset= train)
coef(glm.fit) # same results as lm.fit
coef(lm.fit)

library(boot)
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta # gives us the LOOCV value

## We can repeat the process for increasingly complex polynomial fits
cv.err <- rep(0, 10)

for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.err[i] <- cv.glm(Auto, glm.fit)$delta[1]
}

print(cv.err)
which.min(cv.err) # minimum LOOCV error
cv.err[7]

### K-Fold Cross Validation

set.seed(17)
cv.err.10 <- rep(0, 10)

for (i in 1:10) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.err.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}

cv.err.10

### The Boostrap
### We need 2 steps only : 1) create a function to compute the statistics of interest
### 2) use boot() function, which is part of boot library to perform the boostrap

alpha.fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2*cov(X, Y))
}

alpha.fn(Portfolio, 1:100) # estimate alpha using all of the observations

set.seed(7)
alpha.fn(Portfolio, sample(100, 100, replace = T)) # randomly select 100 observations from the range 1 - 100, with replacement

## We can implement bootstrap analysis by implementing this function many times, record all of alphas and compute standard deviation
## However, boot command automates this approach

boot(Portfolio, alpha.fn, R = 1000)

### estimating quadratic regression coefficients with bootstrap

boot.fn <- function(data, index) {
  coef(
    lm(mpg ~ poly(horsepower, 2), data = data, subset = index)
  )
}

set.seed(1)
boot(Auto, boot.fn, 1000)

### Bootstrapping in general is more computationally expensive but gives more reliable estimates

















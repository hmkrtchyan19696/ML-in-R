
### Best Subset Selection
library(ISLR2)
View(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters <- na.omit(Hitters) # removes all rows that have NA 
dim(Hitters)
sum(is.na(Hitters))

library(leaps) # package for best subset selection

regfit.full <- regsubsets(Salary ~ ., Hitters) # By default reports results up to the best eight-variable model
summary(regfit.full)

regfit.full.2 <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19) # nvmax specifies up to how many variables to include
reg.summary <-summary(regfit.full.2)

names(reg.summary) # what is included in the summary
reg.summary$rsq

## plotting RSS, Adjusted R-squared
par(mfrow = c(1,1))
plot(reg.summary$rss, xlab = "Number of variables", ylab = "RSS", type = 'l')
plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted R-squared", type = 'l')
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], 
       col = 'red', cex = 2, pch = 20) # puts point on already created plot

## plotting Cp, BIC
plot(reg.summary$cp, xlab = "Number of variables", ylab = "Cp", type = 'l')
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)],
       col = 'yellow', cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type = 'l')
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)],
       col = 'green', cex = 2, pch = 20)

## regsubsets has also built in plot() command
plot(regfit.full.2, scale = "r2")
plot(regfit.full.2, scale = "adjr2")
plot(regfit.full.2, scale = "Cp")
plot(regfit.full.2, scale = "bic")

coef(regfit.full.2, 6) # because the best model had 6 variables

### forward and backward stepwise selection

regfit.fwd <- regsubsets(Salary ~ ., data = Hitters ,
                         nvmax = 19, method = "forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary ~ ., data = Hitters ,
                           nvmax = 19, method = "backward")
summary(regfit.bwd)

### cross-validation example

set.seed (1)
train <- sample(c(TRUE , FALSE), nrow(Hitters),
                  replace = TRUE) # randomly choose from true or false by the number of rows in Hitters
test <- (!train)

regfit.best <- regsubsets(Salary ~ .,
                          data = Hitters[train , ], nvmax = 19)

test.mat <- model.matrix(Salary ~ ., data = Hitters[test, ])

val.errors <- rep(NA, 19) 

for (i in 1:19) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}

coef(regfit.best, which.min(val.errors))

### function for automating prediction with regsubsets()
predict.regsubsets <- function(object , newdata , id, ...) {
  form <- as.formula(object$call [[2]])
  mat <- model.matrix(form , newdata)
  coefi <- coef(object , id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

## Finally, we use the 7 model on the full dataset
regfit.best <- regsubsets(Salary ~ ., data = Hitters ,
                          nvmax = 19)
coef(regfit.best , 7)


### k-fold cross-validation example
k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 19,
                    dimnames = list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit <- regsubsets(Salary ~ ., data = Hitters[folds != j, ], nvmax = 19)
  
  for (i in 1:19) {
    pred <- predict(best.fit, Hitters[folds == j, ], id = i) # pred here will call our function
    cv.errors[j, i] <- mean((Hitters$Salary[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

par(mfrow = c(1, 1))
plot(mean.cv.errors , type = "b") # so, the 10 variable model looks the best

reg.best <- regsubsets(Salary ~ ., data = Hitters ,
                       nvmax = 19)
coef(reg.best , 10)

### Ridge and LASSO

x <- model.matrix(Salary ~ ., Hitters)[, -1] # matrix of everything besides the response variable
## model.matrix also automatically creates dummies for qualitative variables
y <- Hitters$Salary

### Ridge : in glmnet if alpha is set to zero then a ridge regression will be performed, if it's set to 1, then LASSO
library(glmnet)
grid <- 10^seq(10, -2, length = 100) # by default glmnet will perform ridge regression with automatically selected lambda values
### we want to test for values ranging from λ = 1010 to λ = 10−2
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid) # glmnet automatically standardizes the values

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2)) # L2 norm

predict(ridge.mod , s = 50, type = "coefficients")[1:20, ] # here, s is the new value of lambda
# and predict function here is used to check the coefficients of variables with lambda 50

## choose lambda with cross-validation
set.seed (1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min

ridge.pred <- predict(ridge.mod , s = bestlam ,
                      newx = x[test , ]) ## here we make actual predictions by stating newx instead of coefficients
### Finally, we fit the whole model and using the best lambda
ridge.final <- glmnet(x, y, alpha = 0,  lambda = bestlam)
predict(ridge.final, type = 'coefficients')[1:20, ]

### LASSO
set.seed (1)
train <- sample (1: nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

lasso.mod <- glmnet(x[train , ], y[train], alpha = 1,
                    lambda = grid)
plot(lasso.mod)

## cross-validation
set.seed (1)
cv.out <- cv.glmnet(x[train , ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod , s = bestlam ,
                        newx = x[test , ])
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out , type = "coefficients",
                        s = bestlam)[1:20, ]
lasso.coef[lasso.coef != 0] # non-zero coefficients

### Principal Component Regression (PCR)
library(pls)
set.seed (2)
pcr.fit <- pcr(Salary ~ ., data = Hitters , scale = TRUE , # standardizing
                 validation = "CV") # 10 fold cross validation for each possible M principal components used
summary(pcr.fit)
validationplot(pcr.fit , val.type = "MSEP") # plotting cross validation scores

### Cross validation
set.seed (1)
pcr.fit <- pcr(Salary ~ ., data = Hitters , subset = train ,
                 scale = TRUE , validation = "CV")
validationplot(pcr.fit , val.type = "MSEP")
# Now we find that the lowest cross-validation error occurs when M = 5
# components are used. We compute the test MSE as follows.
pcr.pred <- predict(pcr.fit , x[test , ], ncomp = 5)
mean (( pcr.pred - y.test)^2)

### Fit the data along all of the data
pcr.fit <- pcr(y ~ x, scale = TRUE , ncomp = 5)
summary(pcr.fit)

### Partial Least Squares (PLS)
set.seed (1)
pls.fit <- plsr(Salary ~ ., data = Hitters , subset = train ,
                  scale = TRUE , validation = "CV")
summary(pls.fit)
validationplot(pls.fit , val.type = "MSEP")

pls.pred <- predict(pls.fit , x[test , ], ncomp = 1)

### Fitting the model on all of the data 
pls.fit <- plsr(Salary ~ ., data = Hitters , scale = TRUE ,
                ncomp = 1)

summary(pls.fit)

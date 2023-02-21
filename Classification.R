
LoadLibraries <- function() {
  library(ISLR2)
  library(MASS)
  print("ISLR and MASS libraries has been loaded")
}

LoadLibraries()

data("Smarket")
names(Smarket)
summary(Smarket)
cor(Smarket[, -9])
plot(Smarket$Volume)

### Logistic Regression
glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
                data = Smarket, family = "binomial")
summary(glm.fits)
glm.probs <- predict(glm.fits, type = "response") # outputs the probability values on the training data
glm.probs[1:10]
contrasts(Smarket$Direction) # what direction is associated with what value

glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > 0.5] = "Up" # converting from probabilities to predictions

table(glm.pred, Smarket$Direction) # creating confusion matrix

### Training and testing sets
train <- (Smarket$Year < 2005)
Smarket.2005 <- Smarket[!train, ]
Direction.2005 <- Smarket$Direction[!train]

glm.fits <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
                data = Smarket, family = "binomial", subset = train)

glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up" # converting from probabilities to predictions

table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

### Linear Discriminant Analysis -- part of MASS library
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, Direction.2005)


### Quadratic Discriminant Analysis -- also part of MASS library
qda.fit <- qda(Direction ~ Lag1 + Lag2 , data = Smarket, subset = train)
qda.fit
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)

### Naive Bayes -- part of e1071 library
library(e1071)

nb.fit <- naiveBayes(Direction ~ Lag1 + Lag2 , data = Smarket ,
                     subset = train)
nb.fit
nb.class <- predict(nb.fit, Smarket.2005)
table(nb.class , Direction.2005)
nb.preds <- predict(nb.fit, Smarket.2005, type = "raw") # returns probabilities

### KNN -- part of class library
library(class)
attach(Smarket)
train.X <- cbind(Lag1 , Lag2)[train , ]
test.X <- cbind(Lag1 , Lag2)[!train , ]
train.Direction <- Direction[train]

set.seed (1)
knn.pred <- knn(train.X, test.X, train.Direction , k = 3)
table(knn.pred , Direction.2005)

dim(Caravan)
attach(Caravan)
summary(Purchase)

standardized.X <- scale(Caravan[, -86]) # standardize the variables to zero mean and sd of one

test <- 1:1000
train.X <- standardized.X[-test, ]
test.X <- standardized.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]

set.seed (1)
knn.pred <- knn(train.X, test.X, train.Y, k = 5)

table(knn.pred, test.Y)


### Poisson Distribution
attach(Bikeshare)
names(Bikeshare)
mod.lm <- lm(
  bikers ~ mnth + hr + workingday + temp + weathersit ,
  data = Bikeshare
)
summary(mod.lm)

contrasts(Bikeshare$hr) = contr.sum(24)
contrasts(Bikeshare$mnth) = contr.sum (12)
mod.lm2 <- lm(
  bikers ~ mnth + hr + workingday + temp + weathersit ,
  data = Bikeshare
)
summary(mod.lm2)

coef.months <- c(coef(mod.lm2)[2:12],
                 -sum(coef(mod.lm2)[2:12]))
plot(coef.months , xlab = "Month", ylab = "Coefficient",
     xaxt = "n", col = "blue", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A",
                                     "M", "J", "J", "A", "S", "O", "N", "D"))


coef.hours <- c(coef(mod.lm2)[13:35] ,
                -sum(coef(mod.lm2)[13:35]))
plot(coef.hours , xlab = "Hour", ylab = "Coefficient",
       col = "blue", pch = 19, type = "o")


mod.pois <- glm(
  bikers ~ mnth + hr + workingday + temp + weathersit ,
  data = Bikeshare , family = poisson
)
summary(mod.pois)


coef.mnth <- c(coef(mod.pois)[2:12] ,
               -sum(coef(mod.pois)[2:12]))
plot(coef.mnth , xlab = "Month", ylab = "Coefficient",
       xaxt = "n", col = "blue", pch = 19, type = "o")
axis(side = 1, at = 1:12, labels = c("J", "F", "M", "A", "M",
                                       "J", "J", "A", "S", "O", "N", "D"))
coef.hours <- c(coef(mod.pois)[13:35] ,
                -sum(coef(mod.pois)[13:35]))
plot(coef.hours , xlab = "Hour", ylab = "Coefficient",
       col = "blue", pch = 19, type = "o")

plot(predict(mod.lm2), predict(mod.pois , type = "response"))
abline (0, 1, col = 2, lwd = 3)


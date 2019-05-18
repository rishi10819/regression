library(tidyverse)
library(MASS)
library(caret)

set.seed(1)

Sigma <- 9 * matrix(c(1, 0.5, 0.5, 1), 2, 2)

dat <- mvrnorm(n = 100, c(69, 69), Sigma = Sigma) %>%
  data.frame() %>%
  setNames(c('x', 'y'))


################################################
# Start of Answer, for Q-1
################################################

set.seed(1)

test_index <- createDataPartition(dat$y, list = FALSE)
#test_index
train_set <- dat[-test_index,]

test_set <- dat[test_index,]

nrow(train_set) # 48
nrow(test_set)  # 52

# Mean - average
avg <- mean(train_set$y)
avg # 69.24063


# SD - Standard Deviation
sd_1 <- mean((avg - test_set$y) ^ 2)
sd_1 # 7.216571


d <- function(size, means, Sigma, names) {
  
  set.seed(1)
  
  mvrnorm(n = size, mu = means, Sigma = Sigma) %>%
    data.frame() %>%
    setNames(names)
  
}


f <- function(data) {
  
  rmse <- replicate(100, {
    
    test_index <- createDataPartition(data$y, list = FALSE)
    
    train_set <- data[-test_index,]
    
    test_set <- data[test_index,]

    
    # Train a linear model
    
    fit <- lm(y ~ x, data = train_set)
    
    y_hat <- predict(fit, test_set)
    
    
    # Squared loss
    
    sqrt(mean((y_hat - test_set$y) ^ 2))
    
  })
  
  structure(c(mean(rmse), sd(rmse)), names = c('mean', 'sd'))
  
}


data <- d(100, c(69, 69), Sigma, c('x', 'y'))
#data
set.seed(1)

results <- f(data)
#print(data)

results
#      mean            sd 
# 2.4886614     0.1243952 

################################################

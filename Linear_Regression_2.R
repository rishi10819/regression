library(tidyverse)
library(MASS)
library(caret)

set.seed(1)

calc_rmse <- function(n) 
{  
  Sigma <- 9 * matrix(c(1, 0.5, 0.5, 1), 2, 2)  
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>% data.frame() %>% setNames(c('x', 'y'))
  
  rmse <- replicate(n=100, {    
    test_index <- createDataPartition(dat$y, list = FALSE)    
    train_set <- dat[-test_index,]
    test_set <- dat[test_index,]
    
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, test_set)
    
    sqrt(mean((y_hat - test_set$y) ^ 2))  }) 
  list(mean(rmse), sd(rmse))  
}
set.seed(1)
n <- c(100, 500, 1000, 5000, 10000)
sapply(n, calc_rmse) 
f <- sapply(n, calc_rmse)
f
#     [,1]      [,2]       [,3]       [,4]       [,5]      
#[1,] 2.633937  2.604644   2.501221   2.583712   2.586454  
#[2,] 0.1702757 0.07411335 0.05306224 0.02528301 0.01704065

library(tidyverse)
library(MASS)
library(caret)

set.seed(1)

#Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3) # Q-6
 Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3) # Q-8

dat <- mvrnorm(n = 100, c(0, 0, 0), Sigma) %>% 
  data.frame() %>% 
  setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

# best model should have lowest RMSE

fit_x_1   <- lm(y ~ x_1,       data = train_set) 
fit_x_2   <- lm(y ~ x_2,       data = train_set)
fit_x_1_2 <- lm(y ~ x_1 + x_2, data = train_set)

y_hat_x_1   <- predict(fit_x_1,   test_set)
y_hat_x_2   <- predict(fit_x_2,   test_set)
y_hat_x_1_2 <- predict(fit_x_1_2, test_set)


calc_RMSE <- function(yhat) mean(sqrt((yhat - test_set$y)^2))

predict_List <- list(x1 = y_hat_x_1, x2 = y_hat_x_2, x_1_2 = y_hat_x_1_2)

list_Of_RMSE <- lapply(predict_List, calc_RMSE)

vector_Of_RMSE <- structure(unlist(list_Of_RMSE), names = names(list_Of_RMSE))

vector_Of_RMSE

# Q-8
#        x1         x2      x_1_2 
# 0.5200316  0.4957970  0.5201700 


# Q-6
#        x1         x2      x_1_2 
# 0.4976172  0.4929421  0.2376018 

best_Model_lowest_RMSE <- names(which.min(vector_Of_RMSE))

cat('The best model with lowest RMSE - ', best_Model_lowest_RMSE, '\n') 

library(tidyverse)
library(caret)

set.seed(2)

make_data <- function(n       = 25, p       = 0.5, 
                      mu_0    = 0,  mu_1    = 2, 
                      sigma_0 = 1,  sigma_1 = 1)
{
  y   <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x   <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test  = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}

dat <- make_data()
dat
dat$train %>%
  ggplot(aes(x, color = y)) +
  geom_density()

delta <- seq(0, 3, len = 25)

##########################################################
# Start by calculating accuracy using given dataset
# Then we will use it solve Q_1 problem
##########################################################

# training the model
fit <- glm(y ~ x, data = dat$train, family = 'binomial')

p_hat <- predict(fit, dat$test)

y_hat <- factor(ifelse(p_hat > 0.5, '1', '0'))

confusion_Matrix <- confusionMatrix(y_hat, reference = dat$test$y)

confusion_Matrix$overall['Accuracy']

##########################################################
# Use of above to solve Q_1
##########################################################

res <- map_dbl(delta, function(x) {
  
  dat   <- make_data(mu_1 = x)
  
  fit   <- glm(y ~ x, data = dat$train, family = 'binomial')
  
  p_hat <- predict(fit, dat$test, type = 'response')
  
  y_hat <- factor(ifelse(p_hat > 0.5, '1', '0'))
  
  confusion_Matrix   <- confusionMatrix(y_hat, reference = dat$test$y)
  
  confusion_Matrix$overall['Accuracy']

})

qplot(delta, res)

##########################################################
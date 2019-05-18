install.packages("HistData")

library(tidyverse)
library(HistData)

#getwd()
#setwd("C:/Edx/projects")
#getwd()

data("GaltonFamilies")
head(GaltonFamilies)

galton_heights <- GaltonFamilies %>%
  filter (childNum == 1 & gender == "male") %>%
  select ( father, childHeight ) %>%
  rename ( son = childHeight)

head(galton_heights)
#print(galton_heights)

galton_heights %>% 
  summarize (mean (father), sd (father), mean (son), sd (son))

galton_heights %>% ggplot (aes(father, son)) +
  geom_point(alpha = 0.5)


galton_heights %>% 
  summarize (cor(father, son))  

#######################################################

B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

lse %>% summarize(cor(beta_0, beta_1))

#######################################################

B <- 1000
N <- 50

lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})

cor(lse[1,], lse[2,]) 

#######################################################

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth()

################

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

################

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

################

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

#######################################################

# Not tested

lm(formula = son ~ father, data = galton_heights)

beta1 = seq(0, 1, len=nrow(galton_heights))

results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

#######################################################
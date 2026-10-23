#' regression lab

rm(list = ls())
pacman::p_load(tidyverse)

head(iris)

v_sp <- unique(iris$Species)

v_setosa <- iris %>%
  filter(Species == v_sp[1]) %>%
  lm(Sepal.Width ~ Petal.Width, data = .)

v_versicolor <- iris %>% 
  filter(Species == v_sp[2]) %>%
  lm(Sepal.Width ~ Petal.Width, data = .)

v_virginica <- iris %>% 
  filter(Species == v_sp[3]) %>%
  lm(Sepal.Width ~ Petal.Width, data = .)

# for lapply
list_lm <- lapply(X = v_sp,
                  function(s) {
                    iris %>% 
                      filter(Species == s) %>% 
                      lm(Sepal.Width ~ Petal.Width, data = .)
                  })

# for loop 
fit <- list(NULL)

for (i in 3:length(v_sp)) {
  fit[[i]] <- iris %>% 
    filter(Species == v_sp[i]) %>% 
    lm(Sepal.Width ~ Petal.Width, data = .)
}

## 12.4.2 Multiple explanatory variables
v_setosa2 <- iris %>% 
  filter(Species == v_sp[1]) %>%
  lm(Sepal.Width ~ Petal.Width + Petal.Length, data = .)

v_versicolor2 <- iris %>% 
  filter(Species == v_sp[2]) %>%
  lm(Sepal.Width ~ Petal.Width + Petal.Length, data = .)

v_virginica2 <- iris %>% 
  filter(Species == v_sp[3]) %>%
  lm(Sepal.Width ~ Petal.Width + Petal.Length, data = .)

# when adding another explanatory variable, the estimate of regression
# coefficients drop, and the coefficients of determination are less than
# a model with only one single explanatory variable. 

iris_x <- iris %>% 
  mutate(useless = rnorm(150))

m0 <- lm(Sepal.Width ~ Petal.Width, data = iris_x)
m1 <- lm(Sepal.Width ~ Petal.Width + useless, data = iris_x)


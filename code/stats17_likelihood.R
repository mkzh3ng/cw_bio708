#' Likelihood

pacman::p_load(tidyverse,
               patchwork)

rm(list = ls())

# review: 
## remember how residuals were used to find intercept  
## and slope of line of best fit!

## poisson distribution is bound by 0


# poisson example of likelihood -------------------------------------------

## probability of observing 3 given lambda = 3.5
(pr3 <- dpois(3, lambda = 3.5))

## try 0.1 through 10 as possible values of lambda (intervals of 0.1)
lambda <- seq(0.1, 10, by = 0.1)

pr <- dpois(3, lambda = lambda)

df_pois <- tibble(lambda = lambda, y = 3, pr = pr) %>% 
  arrange(desc(pr))

## vector case y = {3, 2, 5}
## probability of observing 3 data points togather
## use lambda = 3.5
y = c(3, 2, 5)
pr2 <- dpois(y, lambda = 3.5)

# manual calculation
(lik <- pr[1] * pr[2] * pr[3])

# product
lik <- prod(pr)

# likelihood for each lambda value
# prob of observing vector y
lik_i <- sapply(lambda, 
                function (x){
                  prod(dpois(y, lambda = x))
                })

df_pois3 <- tibble(
  lambda = lambda,
  lik = lik_i) %>% 
  arrange(desc(lik))

## sample mean of y
mean(y)

## why sample mean matches the MLE
## Pr(y = 3) with lambda = 3.5
dpois(3, lambda = 3.5)
(3.5^3 * exp(-3.5))/factorial(3)

## visualization
df_pois3 %>% 
  ggplot(
    aes(x = lambda, 
        y = log(lik))
  ) + 
  geom_line() + 
  labs(x = "lambda value",
       y = "Likelihood")

## get likelihoof from glm 
df_count <- read_csv("data_src/data_garden_count.csv") 

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = poisson)

logLik(m_pois)

summary(m_pois)

## the assumption of likelihood is that every sample is independant from each 
## other! Since compounding variables aren't considered. 
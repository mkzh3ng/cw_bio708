#' linear regression 

rm(list = ls())
pacman::p_load(tidyverse)


# data structure ----------------------------------------------------------

df_algae <- read_csv("data_src/data_algae.csv")

df_algae %>% 
  ggplot(
    aes(x = conductivity,
        y = biomass)
  ) +
  geom_point()

m <- lm(biomass ~ conductivity,
        data = df_algae)

summary(m)

# std. error is the standard deviation of line, (sigma) 

## extract intercept and slope
alpha <- coef(m)[1]
beta <- coef(m)[2]

df_algae %>% 
  ggplot(
    aes(x = conductivity, 
        y = biomass)) + 
  geom_point() + 
  geom_abline(intercept = alpha,
              slope = beta) 

# regression null hypothesis is b = 0 (no relationship between x and y)

0.50355/0.02568 # estimate/std error = t-value

## extract coef
theta <-coef(m)
se <- sqrt(diag(vcov(m)))
(t_value <- theta/se)

# unexplained variation ---------------------------------------------------

## extract residual 
eps0 <- df_algae$biomass - (alpha + beta * df_algae$conductivity)
eps <- resid(m)
plot(eps0~eps) 

## visualize error
df_algae <- df_algae %>% 
  mutate(eps = eps)

df_algae %>% 
  ggplot(
    aes(x =conductivity,
        y = biomass)
  ) + 
  geom_point() + 
  geom_abline(
    intercept = alpha, 
    slope = beta
  ) + 
  geom_segment(
    aes(
      x = conductivity,
      xend = conductivity,
      y = biomass, 
      yend = biomass -eps
    ),
    linetype = "dashed"
  )

## original variation in y
y <- df_algae$biomass
ss0 <- sum((y - mean(y))^2)

ss1 <- sum((eps)^2)

rsq <-1 - (ss1/ss0) # how much variation remains unexplained (multiple r-squared)

summary(m)





rm(list = ls())
pacman::p_load(tidyverse,
               patchwork)


# normality assumption ---------------------------------------------------------------------

ToothGrowth <- as_tibble(ToothGrowth)
distinct(ToothGrowth, len)

df_tooth <- ToothGrowth

m_tooth <- lm(len ~ supp * dose,
              data = df_tooth)

eps <- resid(m_tooth)
shapiro.test(eps) # test to see if residuals are NOT following normal distrib.
# so if we see a value lower than 0.05, it does follow null hypothesis

# model interpretation ----------------------------------------------------

df_pred <- ToothGrowth %>%
  group_by(supp) %>%
  reframe(dose = seq(min(dose),
                     max(dose),
                     length = 100))

y_pred <- predict(m_tooth,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

y_pred_int <- predict(m_tooth,
                      newdata = df_pred)

df_tooth %>% 
  ggplot(aes(
    x = dose,
    y = len,
    color = supp
  )) + 
  geom_point(alpha = 0.5) + 
  geom_line(data = df_pred,
            aes(y = y_pred_int))

# Multicollinearity -------------------------------------------------------

## variance-covariance matrix
mv <- rbind(c(1, 0.9),
            c(0.9, 1))

## true regression coefficients
b <- c(0.05, 1.00)

## produce simulated data
set.seed(523)
X <- MASS::mvrnorm(100,
                   mu = c(0, 0),
                   Sigma = mv)

df_y <- tibble(x1 = X[,1],
               x2 = X[,2]) %>% 
  mutate(y = rnorm(nrow(.),
                   mean = 1 + b[1] * x1 + b[2] * x2))

x1_plot <- df_y %>% 
  ggplot(
    aes(
      x = x1,
      y = y)) + 
  geom_point()
  
x2_plot <- df_y %>% 
  ggplot(
    aes(
      x = x2,
      y = y)) + 
  geom_point()

x1_plot + x2_plot

## both seem to have a positive influence on y

xy_comp <- lm(y ~ x1 + x2, data = df_y)
summary(xy_comp)

xy_plot <- df_y %>% 
  ggplot(
    aes(
      x = x1,
      y = x2)) + 
  geom_point()

cor(df_y$x1, df_y$x2)

# experimental design -----------------------------------------------------

## we can see the effects of nutrients and predation but in the presence 
## of high nutrients. We are missing the low nutrient, predator absent data.

#' two-group comparison: t-test

pacman::p_load(tidyverse)
rm(list=ls())

# read fish length data
df_fl <- read_csv("data_src/data_fish_length.csv")

# base r function
unique(df_fl$lake)

# dplyr
distinct(df_fl,lake)

# get mean and sd body size
df_fl_mu <- df_fl %>% 
  group_by(lake) %>% 
  summarize(mu_l = mean(length),
            sd_l = sd (length)
            )

# figure 
df_fl %>% 
  ggplot(
    aes(x = lake, 
        y = length)
  ) +
  geom_jitter(
    width = 0.1,
    height = 0, 
    alpha = 0.25
  ) +
  geom_segment(
    data = df_fl_mu,
    aes(
      x = lake, 
      xend = lake, 
      y = mu_l - sd_l,
      yend = mu_l + sd_l)
  ) +
  geom_point(
    data = df_fl_mu,
    aes(
      x = lake, 
      y = mu_l)
  )

# t-test 
x <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

y <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

t.test(x, y, var.equal = TRUE)

# get t-value
v_mu <- df_fl_mu %>% 
  pull(mu_l)

v_mu[1] - v_mu[2]

df_t <- df_fl %>% 
  group_by(lake) %>% 
  summarize(
    mu_l = mean(length),
    var_l = var(length),
    n = n()
  )

# mean vector
v_mu <- pull(df_t, mu_l)

# variance vector
v_var <- pull(df_t, var_l)

# sample size vector
v_n <- pull(df_t, n)

# pooled variance
var_p <- ((v_n[1] - 1) / (sum(v_n) - 2)) * v_var[1] +
  ((v_n[2] - 1) / (sum(v_n) - 2)) * v_var [2]

t_value <- (v_mu[1] - v_mu[2]) / sqrt(var_p * ((1/v_n[1]) + (1 / v_n [2])))

# t value increases when reliability of difference is larger
# null hypothesis of t-test is p value = zero 

# getting p value
x <- seq(-5, 5, length = 500)

# probability density of t-statistic with df = 98
y <- dt(x, df = sum(v_n) - 2)
y1 <- dt(x, df = 10 - 2)

tibble(x,y,y1) %>% 
  ggplot(
    aes(
      x = x, 
      y = y, 
    )
  ) + 
  geom_line() +
  geom_line(aes(y = y1), 
            color = "red") +
  geom_vline(xintercept = abs(t_value)) + 
  geom_vline(xintercept = t_value) + 
  labs(y = "Probability density",
       x = "t-statistic")
  
# t-test under unequal variance
x <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

y <- df_fl %>% 
  filter(lake == "b") %>% 
  pull(length)

t.test(x,y,var.equal = FALSE)



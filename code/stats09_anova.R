#' more than two groups (ANOVA)

pacman::p_load(tidyverse)
rm(list = ls())

## dataset
df_anova <- read_csv("data_src/data_fish_length_anova.csv")
distinct(df_anova,lake)

df_anova %>% 
  ggplot(
    aes(x = lake,
        y = length)
  ) +
  geom_violin(draw_quantiles = 0.5, # draw median horizontal line
              alpha = 0.2) + # transparency
  geom_jitter(height = 0, # transparency
              width = 0.1,
              alpha = 0.2) 

## anova
aov(length ~ lake, # put variable on the left the thing you want to explain
    data = df_anova) # right variable is the thing that explains left

## anova details
## overall mean
mu <- mean(df_anova$length)

## group specific means (amt variation by calling difference between groups)
df_g <- df_anova %>% 
  group_by(lake) %>% 
  summarize(mu_g = mean(length),
            dev_g = (mu_g-mu)^2,
            n = n())

ss_b <- df_g %>%
  mutate(ss_g = dev_g*n) %>% 
  pull(ss_g) %>% 
  sum()

# gives you sum of squares for lakes in anova test!

## within group variability
ss_w <- df_anova %>% 
  group_by(lake) %>% 
  mutate(mu_g = mean(length)) %>% 
  ungroup() %>% 
  mutate(dev_i = (length - mu_g)^2) %>% 
  pull(dev_i) %>% 
  sum()

## overall variability
ss_o <- sum((df_anova$length - mu)^2)
ss_b + ss_w # same thing!


## convert variability to "variance"
summary(aov(length~lake, df_anova))
sig_b <- ss_b/2
sig_w <- ss_w/(nrow(df_anova)-n_distinct(df_anova$lake))

## test statistic
f_value <- sig_b/sig_w # tells us if difference between groups are significant

# anova is usually combined with post-hoc test since it only proves groups
# are statistically different, but which groups exactly?

## null distribution 
f <- seq(0,10, by = 0.01)
probdistrib <- df(f, df1 = 2, df2 = 147)

tibble(x = f, y = probdistrib) %>% 
  ggplot(
    aes(x = x,
        y = y)
    ) + 
  geom_line() + 
  geom_vline(xintercept = f_value,
             color = "darkseagreen") + 
  labs(x = "F statistic",
       y = "probability density")

p_value <- 1 - pf(f_value, df1 = 2, df2 = 147)  


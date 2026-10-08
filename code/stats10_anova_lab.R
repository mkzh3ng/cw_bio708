#' more than two groups anova

## question 1:
pacman::p_load(tidyverse)
rm(list = ls())

df_plant <- PlantGrowth

df_plant %>% 
  mutate(
    treatment_group = case_when(
      group == "crtl" ~ "Control",
      group == "trt1" ~ "Treatment 1",
      group == "trt2" ~ "Treatment 2"
    )
  ) %>% 
  ggplot(aes(x = treatment_group,
             y = weight)) +
  geom_violin(draw_quantiles = 0.5, 
              alpha = 0.2) + 
  geom_jitter(width = 0.2,
              alpha = 0.2) +
  labs(
    x = "Treatment Group",
    y = "Weight"
  )

## question 2: 
plant <- aov(formula = weight ~ group,
             data = df_plant)

summary(plant)

# p value is below 0.05, so it is statistically different

## question 3: 
# Values to be recorded include the test statistic (F Value), the P value, 
# and the Degrees of Freedom

# power analysis ----------------------------------------------------------
## install.packages("pwr")

pwr::pwr.anova.test(
  k = 3,
  f = 0.5, 
  sig.level = 0.05, 
  power = 0.8
)
# k = number of groups
# f = effect size (0.1 = small, 0.4+ = large)
# sig.level = significance level (0.05)
# power = desired statistical power (usually 0.8)

# n = 13.89521, or 14 individuals per group, or 42 in total 

pwr::pwr.anova.test(
  k = 3,
  f = 0.5, 
  sig.level = 0.05, 
  n = 40
)
# as sample size increases, power also increases!

pwr::pwr.anova.test(
  k = 3,
  f = 0.4, 
  sig.level = 0.05, 
  n = 14
)
# as effect size decreases, power decreases as well!

pwr::pwr.anova.test(
  k = 2,
  f = 0.5, 
  sig.level = 0.05, 
  n = (13.89521*1.5)
)
# as groups decrease but total sample size stays the same, the power increases
# therefore with less groups and greater sample size, more power. 
# Sampling 

rm(list=ls()) #clears environment

## install.packages("pacman")
pacman::p_load(tidyverse)
library(tidyverse)

## data frame 1

h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)
df_h1 <- tibble(plant_id = 1:10,
                height = h,
                unit = "cm")

df_h1 <- df_h1 %>% 
  mutate(mu_height = mean(h),
         var_height = sum((h-mean(h))^2/nrow(.)))

## data frame 2

h2 <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20, # a vector from 11 to 20 by 1
                height = h2,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

print(df_h2)

## parameter is an unmeasurable constant, but can be estimated from samples

df_h0 <- read_csv("data_src/data_plant_height.csv")
df_h0

## true mean
mu <- mean(df_h0$height)
mu

## true varience
sigma2 <- sum((df_h0$height - mu)^2)/nrow(df_h0)
sigma2

## random sampling 
df_i <- df_h0 %>% 
  sample_n(size = 10)

## for loop
set.seed(3)

mu_i <- var_i <- NULL

for (i in 1:1000) {
  df_i <- df_h0 %>% 
    sample_n(size = 10)
  
  ## mean for a subset
  mu_i[i] <- mean(df_i$height)
  
  ## varience for a subset
  var_i[i] <- sum((df_i$height - mu_i[i])^2)/nrow(df_i)
}

pacman::p_load(tidyverse,
               patchwork)

df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i)

g_mu <- df_sample %>% 
  ggplot(aes(x = mu_hat))+
  geom_histogram()+
  geom_vline(xintercept = mu)
g_mu

g_var <- df_sample %>% 
  ggplot(aes(x = var_hat))+
  geom_histogram()+
  geom_vline(xintercept = sigma2)
g_var

## how patchwork works 
g_hor <- g_mu + g_var
g_ver <- g_mu / g_var

# bias-corrected version --------------------------------------------------

## for loop
mu_i <- var_ub_i <- NULL

for (i in 1:1000) {
  df_i <- df_h0 %>% 
    sample_n(size = 10)
  
  ## mean for a subset
  mu_i[i] <- mean(df_i$height)
  
  ## varience for a subset
  ##var()'s denominator is N-1, not N
  var_ub_i[i] <- sum((df_i$height - mu_i[i])^2)/nrow(df_i)
}

## adding a new column to df_sample
df_sample <- df_sample %>% 
  mutate(var_ub_hat = var_ub_i)

g_var_ub <- df_sample %>% 
  ggplot(aes(x = var_ub_hat))+
  geom_histogram()+
  geom_vline(xintercept = sigma2)

## combined figure
g_mu / g_var / g_var_ub

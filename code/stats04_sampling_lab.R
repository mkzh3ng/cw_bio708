
# laboratory question -----------------------------------------------------

library(tidyverse)
library(patchwork)

df_h0 <- read_csv("data_src/data_plant_height.csv")

df_50 <- df_h0 %>% 
  sample_n(size = 50) 

df_100 <- df_h0 %>% 
  sample_n(size = 100)

mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

mu_i50 <- var_i50 <- var_ub_i50 <- NULL

mu_i100 <- var_i100 <- var_ub_i100 <- NULL

for (i in 1:1000){
  
  df_50 <- df_h0 %>% 
    sample_n(size = 50) 
  
  mu_i50[i] <- mean(df_50$height)

  var_ub_i50[i] <- var(df_50$height)
  
  df_100 <- df_h0 %>% 
    sample_n(size = 100) 
  
  mu_i100[i] <- mean(df_100$height)
  
  var_ub_i100[i] <- var(df_100$height)
  
}
  
df_sample <- tibble(mu_hat50 = mu_i50,
                    var_ub_hat50 = var_ub_i50,
                    mu_hat100 = mu_i100,
                    var_ub_hat100 = var_ub_i50)

g_mu50 <- df_sample %>% 
  ggplot(aes(x = mu_hat50)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

g_var_ub50 <- df_sample %>% 
  ggplot(aes(x = var_ub_hat50)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(10,50))
                     
g_mu100 <- df_sample %>% 
  ggplot(aes(x = mu_hat100)) +
  geom_histogram() +
  geom_vline(xintercept = mu) 

g_mu100

g_var_ub100 <- df_sample %>% 
  ggplot(aes(x = var_ub_hat100)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(10,50))
                     
(g_mu50/g_var_ub50)|(g_mu100/g_var_ub100)


# question 2 --------------------------------------------------------------

df_h10 <- df_h0 %>% 
  filter(height >= 10)

df_50d <- df_h10 %>% 
  sample_n(size = 50) 

df_100d <- df_h10 %>% 
  sample_n(size = 100)

mu <- mean(df_h0$height)
sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h10)

mu_50d <- var_ub50d <- NULL

mu_100d <- var_ub100d <- NULL

for (i in 1:1000){
  
  df_50d <- df_h10 %>% 
    sample_n(size = 50) 
  
  mu_50d[i] <- mean(df_50d$height)
  
  var_ub50d[i] <- var(df_50d$height)
  
  df_100d <- df_h10 %>% 
    sample_n(size = 100) 
  
  mu_100d[i] <- mean(df_100d$height)
  
  var_ub100d[i] <- var(df_100d$height)
  
}

df_sampled <- tibble(mu_hat50d = mu_50d,
                    var_ub50d = var_ub50d,
                    mu_hat100d = mu_100d,
                    var_ub100d = var_ub100d)

g_mu50d <- df_sampled %>% 
  ggplot(aes(x = mu_50d)) +
  geom_histogram() +
  geom_vline(xintercept = mean(df_h0$height))

g_var50d <- df_sampled %>% 
  ggplot(aes(x = var_ub50d)) +
  geom_histogram() 
                     
g_mu100d <- df_sampled %>% 
  ggplot(aes(x = mu_100d)) +
  geom_histogram() +
  geom_vline(xintercept = mean(df_h0$height))

g_var100d <- df_sampled %>% 
  ggplot(aes(x = var_ub100d)) +
  geom_histogram() 

(g_mu50d/g_mu50)|(g_var50d/g_var_ub50)
                     



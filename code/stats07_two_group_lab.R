#' two group comparison lab

pacman::p_load(tidyverse)
rm(list=ls())

## 1) create the following vectors with rnorm()
xs <- rnorm(mean = 10, sd = 5, n = 10)
ys <- rnorm(mean = 12, sd = 5, n = 10)
xl <- rnorm(mean = 10, sd = 5, n = 100)
yl <- rnorm(mean = 12, sd = 5, n = 100)

# perform t-test for these vectors
t.test(xs, ys, var.equal = TRUE)
# df = 18, p-value = 0.144

t.test(xl, yl, var.equal = TRUE)
# df = 198, p-value = 0.001491

## 2) We have 4 vectors: a1, a2, b1, b2
a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

# 2.1) Estimate sample means and SDs for each vector.
df_ab <- tibble(a1 = a1, a2 = a2, b1 = b1, b2 = b2) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "group",
    values_to = "value"
  )

df_mu <- df_ab %>% 
  group_by(group) %>% 
  summarize(mu = mean(value),
            sig = sd(value))

df_ab %>% 
  filter(group %in% c("a1", "a2")) %>% 
  ggplot(
    aes(x = group,
        y = value
    )
  ) +
  geom_jitter(
    height = 0, 
    width = 0.1,
    alpha = 0.5
  ) +
  geom_segment(
    data = df_mu %>% 
      filter(group %in% c("a","a2")),
    aes(
      y = mu - sig,
      yend = mu + sig
    )
  ) +
  geom_point(
    data = df_mu %>% 
      filter(group %in% c("a1","a2")),
    aes(y = mu),
    size = 2.5
  )

# Simulate null hypothesis

df_fl <- read_csv("data_src/data_fish_length.csv")
mu <- mean(df_fl$length)
sig <- sd(df_fl$length)

x <- rnorm(n = 50, mean = mu, sd = sig)
y <- rnorm(n = 50, mean = mu, sd = sig)

v <- t.test(x,y, var.equal = TRUE)$statistic
# t = -1.268973

v <- NULL
R <- 50000
for(i in 1:R) {
  x <- rnorm(n = 50, mean = mu, sd = sig)
  y <- rnorm(n = 50, mean = mu, sd = sig)
  v[i] <- t.test(x,y, var.equal = TRUE)$statistic
}

# 4 

a <- df_fl %>% 
  filter(lake == "a") %>% 
  pull(length)

b <- df_fl %>% 
  filter(lake =="b") %>% 
  pull(length)

t_obs <- t.test(a,b, var.equal = TRUE)$statistic

tibble (v = v) %>% 
  ggplot(aes(x = v))+
  geom_histogram ()+
  geom_vline(xintercept = t_obs)+ 
  geom_vline(xintercept = -t_obs)

# 5 
mean(abs(v)>abs(t_obs))
t.test(a,b, var.equal = T)

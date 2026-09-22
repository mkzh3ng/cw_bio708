# Probability distribution

rm(list=ls())
pacman::p_load(tidyverse,
               patchwork)

# continuous variable  ----------------------------------------------------

# load csv data on R
df_h0 <- read_csv("data_src/data_plant_height.csv")

df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width
                 center = 0.5) + # bin's center specification
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean

# draw probability distribution
x <- seq(min(df_h0$height),
    max(df_h0$height),
    length = 100)

mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)
pd <- dnorm(x, mean = mu, sd = sigma)

tibble(y = pd, 
        x = x) %>% 
  ggplot(aes(x = x,
             y = y)
         ) +
  geom_line() +
  labs(y = "Probability density",
       x = "Plant height")

# convert probability density to frequency
p10 <- pnorm(q = 10, mean = mu, sd = sigma) # probability of x < 10
p20 <- pnorm(q = 20, mean = mu, sd = sigma)
p20-p10

x_min <- floor(min(df_h0$height))
x_max <- ceiling(max(df_h0$height))
bin <- seq(x_min, x_max, by = 1)

p <- NULL
for (i in 1:(length(bin)-1)){
  ## p_up - probability up to bin [i+1]
  p_up <- pnorm(bin[i+1], mean = mu, sd = sigma)
  ## p_down - probability up to bin [i]
  p_low <- pnorm(bin[i], mean = mu, sd = sigma)

  ## difference p_up - p_low represents probability between bin[i] and bin [i+1]
  p[i] <- p_up - p_low
}

df_prob <- tibble(p, bin = bin[-length (bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_h0))

# combine data and pdf
df_h0 %>% 
  ggplot(aes(x = height))+
  geom_histogram(
    binwidth = 1,
    center = 0.5
  ) +
  geom_point(
    data = df_prob, 
    aes(x = bin,
        y = freq),
    color = "red"
  ) +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "black")

# discrete variable -------------------------------------------------------

df_count <- read_csv("data_src/data_garden_count.csv")
print(df_count)

# histogram
df_count %>% 
  ggplot(aes(x = count))+
  geom_histogram(
    binwidth = 0.5,
    center = 0
  )

# poisson fit
x <- seq(0, 10, by = 1)

lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)

## figure
tibble(x = x, 
       y = pm) %>% 
  ggplot(aes(x = x, y = y))+
  geom_line(linetype = "dashed")+
  geom_point() +
  labs (y = "probability", 
        x = "count")
  
df_prob <- tibble(x = x,
                  y = pm) %>% 
  mutate(freq = y * nrow(df_count))

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(
    binwidth = 1, 
    center = 0
  )+
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            linetype = "dashed", 
            color = "brown")+
  geom_point(data = df_prob, 
             aes(x = x, 
                 y = freq))


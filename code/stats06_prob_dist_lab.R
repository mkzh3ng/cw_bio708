#' Probability Distribution Lab

pacman::p_load(tidyverse,
               patchwork)


# Normal Distribution -----------------------------------------------------

# generate 50 observations
x <- rnorm(
  n = 50,
  mean = 20,
  sd = 3
)

# get sample mean and sd
mu <- mean(x)
sigma <- sd(x)

# define bins
x_min <- floor(min(x))
x_max <- ceiling(max(x))

bin <- seq(x_min,x_max, by = 1)

# calculate probability for each bin 
p <- NULL
for (i in 1:(length(bin) - 1)) {
  xu <- pnorm(bin[i + 1], mean = mu, sd = sigma)
  xl <- pnorm(bin[i], mean = mu, sd = sigma)
  
  p[i] <- xu-xl
}

# get frequency - p * "50" because we have 50 observations
# or length(x) (safer)
df_prob <- tibble(
  p = p,
  bin = bin[-length(bin)] + 0.5
) %>% 
  mutate(
    freq = p * length(x)
  )

# draw histogram
df_x <- tibble(x = x)

df_x %>% 
  ggplot(aes(x= x)) +
  geom_histogram(
    binwidth = 1, 
    center = 0.5
  ) +
  geom_point(
    data = df_prob, 
    aes(x = bin,
        y = freq),
    color = "purple"
  ) +
  geom_line(
    data = df_prob, 
    aes(x = bin ,y = freq),
    color = "black",
    linetype = "dotted"
  )


# Poisson  Distribution ---------------------------------------------------
# generate poisson-distributed random numbers
z <- rpois(
  n = 1000,
  lambda = 10
)

# bins
bin <- seq(min(z), max(z), by = 1)

# sample mean 
lambda_hat <- mean(z)

pm <- dpois(z, lambda = lambda_hat)

tibble(y = pm, x = z) %>% 
  ggplot(aes(x = x, y = y))+
  geom_line(linetype = "dashed") +
  geom_point() + 
  labs(y = "Probability",
       x = "Count")

df_z <- tibble(z = z)

df_prob <- tibble(x = z, y = pm) %>% 
  mutate(freq = y * nrow(df_z)) # prob x sample size

df_z %>% 
  ggplot(aes(x = z)) +
  geom_histogram(binwidth = 0.5, # must be divisible number of one; e.g., 0.1, 0.25, 0.5...
                 center = 0) +
  geom_line(data = df_prob,
            aes(x = z,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob,
             aes(x = z,
                 y = freq))

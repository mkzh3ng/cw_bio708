## Exercises

z <- exp(rnorm(n=100, mean=0, sd=0.1))
mu1 <- mean(z)
mu2 <- prod(z)^(1/(length(z)))
mu3 <- median(z)

library(ggplot2)
library(tidyverse)
tib_z <- as_tibble(z) %>% 
ggplot(aes(x=z))+
  geom_histogram()+
  geom_vline(xintercept=mu1)+
  geom_vline(xintercept=mu2, color="blue")+
  geom_vline(xintercept=mu3,color = "red")
tib_z

z_rev <- -z+max(z)+0.5
tib_rev <- as_tibble(z_rev) %>% 
ggplot(aes(x = z_rev))+
  geom_histogram()+
  geom_vline(xintercept=mean(z_rev))+
  geom_vline(xintercept=prod(z_rev)^(1/length(z)), color="green")+
  geom_vline(xintercept=median(z_rev),color = "purple")
tib_rev

## Example 2

w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w

m <- w*1000

sd_w <- sqrt(sum((w-mean(w))^2)/length(w))
sd_w

sd_m <- sqrt(sum((m-mean(m))^2)/length(m))
sd_m

mad_w <- median(abs(w-median(w)))
print(mad_w)

mad_m <- median(abs(m-median(m)))
print(mad_m)

# you should use cv for comparing population variation, sd gives unreliable comparison
cv_w <- sd_w/mean(w)
print(cv_w)

cv_m <- sd_m/mean(m)
print(cv_m)

mm_w <- mad_w/median(w)
print(mm_w)

mm_m <- mad_m/median(m)
print(mm_m)
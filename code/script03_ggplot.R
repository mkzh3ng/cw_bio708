
## Call every single time!
library(tidyverse)

## point figure ------------------------------
iris %>% 
  ggplot(
    mapping = aes(x = Sepal.Length,
                  y = Sepal.Width)
  ) +
  geom_point()

## error, no aes()
## shortcut for cutting something out, ctr + shift + c
# iris %>% 
#   ggplot(
#     x = Sepal.Length,
#     y = Sepal.Width
#   ) +
#   geom_point()

## Color by Species
iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width,
             color = Species)) +
  geom_point()

iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width)
        ) +
  geom_point(color = "salmon")


## Error, color argument outside aes()
# iris %>% 
#   ggplot(aes(x = Sepal.Length,
#              y = Sepal.Width,)
#              color = Species
#  ) +
#   geom_point()

# line figure ---------------------------------

df0 <- tibble(x = rep(1:50, 3),
  y = x * 2)

df0 %>% 
  ggplot(
    mapping = aes(x=x,
                  y=y)
  ) +
  geom_line()
  

# Histogram -----------------------------------

iris %>% 
  ggplot(mapping = aes(x = Sepal.Length)) + 
  geom_histogram(color = "purple")

# Box Plot ---------------------------------

iris %>% 
  ggplot(
    mapping = aes(x = Species,
                  y = Sepal.Length)
  ) + 
  geom_boxplot()

## color arg changes the border color
iris %>% 
  ggplot(
    mapping = aes(x = Species,
                  y = Sepal.Length,
                  color = Species)
  ) + 
  geom_boxplot()

## fill arg changes box color
iris %>% 
  ggplot(
    mapping = aes(x = Species,
                  y = Sepal.Length,
                  fill = Species)
  ) + 
  geom_boxplot()

# fun plot ------------------------------

##install.packages("ggridges")
library(ggridges)
library(ggplot2)

iris %>% 
  ggplot(
    mapping = aes(x = Sepal.Length,
                  y = Species,
                  fill = Species)) +
  geom_density_ridges()+
  theme_ridges()+
  theme(legend.position = "none")
#' GLM Lab

pacman::p_load(tidyverse, 
               patchwork)

rm(list = ls())

# GLM exercise ------------------------------------------------------------
url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

# 1: 
m_fish <- glm(n_sp ~ distance + cat_area + hull_area,
              data = df_fish,
              family = "poisson")

summary(m_fish) # distance was significant!

# 2: 
m_cars <- glm(am ~ mpg + hp + wt,
              data = mtcars,
              family = "binomial")

g_cars <-  glm(am ~ mpg + hp + wt,
               data = mtcars,
               family = "gaussian")

summary(m_cars) # weight is significant
summary(g_cars) # hp is significant now too

# Effect size -------------------------------------------------------------
# 1: 
df_fish <- df_fish %>% 
  mutate(std_dist = c(scale(distance)),
         std_area = c(scale(cat_area)),
         std_hull = c(scale(hull_area))
  )

std_pois <- glm(n_sp ~ std_dist + std_area + std_hull,
                data = df_fish,
                family = "poisson")

summary(std_pois)
summary(m_fish)

## Using a scaled variable won't change the z/p value, but will
## change the presentation of the results. The intercept after standardization
## will be expected mean of x?

# offset term ---------------------------------------------------------------
# 1: 
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_offset.csv"
df_offset <- read_csv(url)

## count vs nitrate
g_nitrate <- df_offset %>% 
  ggplot(
    aes(x = nitrate,
        y = count)
  ) + 
  geom_point()

## count vs area
g_area <- df_offset %>% 
  ggplot(
    aes(x = area,
        y = count)
  ) + 
  geom_point()

## count/area vs nitrate
g_density <- df_offset %>% 
  ggplot(
    aes(x = nitrate,
        y = count/area)
  ) + 
  geom_point()

g_nitrate | g_area | g_density


# 2: 
m_pois <- glm(count~nitrate,
              data = df_offset,
              family = "poisson")

summary(m_pois)

o_pois <- glm(count ~ nitrate + offset(log(area)), 
              data = df_offset,
              family = "poisson")

summary(o_pois)

# overdispersion ----------------------------------------------------------
url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_tadpole.csv"
df_tadpole <- read_csv(url)

# 1:
a <- df_tadpole %>% 
  ggplot(
    aes(x = aqveg,
        y = tadpole)
  ) + 
  geom_point()

b <- df_tadpole %>% 
  ggplot(
    aes(x = permanence,
        y = tadpole)
  ) + 
  geom_point()

a|b
# 2:
## maybe permanence? I see more of a trend here 

# 3:
t_pois <- glm(tadpole ~ aqveg + permanence,
              data = df_tadpole,
              family = "poisson")

summary(t_pois)

mean(df_tadpole$tadpole)
var(df_tadpole$tadpole)

## var is much bigger than mean, and poisson model assumes that var is somewhat
## close to mean so this is a case of overdispersion!

# negative-binomial is better
MASS::glm.nb(tadpole ~ aqveg + permanence,
             data = df_tadpole) %>% 
  summary()

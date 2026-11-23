#' model comparison lab

# install.packages("palmerpenguins")
pacman::p_load(tidyverse,
               patchwork,
               palmerpenguins,
               janitor)

rm(list = ls())

# dataset manipulation -----------------------------------------------------------
df_penguins <- clean_names(penguins_raw)

df_penguins <- df_penguins %>% 
  mutate(clutch_completion_b = ifelse(df_penguins$clutch_completion == "Yes",
                                      1, 0))

df_penguins %>% 
  mutate(species = case_when(
    species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
    species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo",
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap"))

## OR 

sp <- unique(df_penguins$species)

df_penguins <- df_penguins %>% 
  mutate(
    species = case_when(species == sp[1] ~ "adelie",
                        species == sp[2] ~ "gentoo",
                        species == sp[3] ~ "chinstrap"
    )
  )

df_penguins <- df_penguins %>% 
  drop_na(
    culmen_length_mm,
    culmen_depth_mm,
    flipper_length_mm,
    body_mass_g
  )

# Analyze penguin data ----------------------------------------------------
## full model for model selection with dredge()
m <- glm(clutch_completion_b ~ 
           species +
           culmen_length_mm + 
           culmen_depth_mm + 
           flipper_length_mm +
           body_mass_g +
           sex, 
         data = df_penguins,
         family = "binomial")

summary(m)

## model selection
# install.packages("MuMIn")
library(MuMIn)
options(na.action = "na.fail")
m_set <- dredge(m, rank = "AIC")
m_sub <- subset(m_set, delta < 2)

list_m <- get.models(object = m_set, subset = delta < 2)
summary(list_m[[1]]) 

## there is significance now, hard to lump all variables things all together. 

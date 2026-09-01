#install.packages ("tidyverse")

library(tidyverse)

set.seed(123)

iris_sub<-as_tibble(iris)%>%
  group_by(Species)%>%
  sample_n(3)%>%
  ungroup()

print(iris_sub)

#Filter

filter(iris_sub, Species == "virginica")

filter(iris_sub, Species != "virginica")

filter(iris_sub, Species %in% c("virginica", "versicolor"))

filter(iris_sub, !(Species %in% c("virginica", "versicolor")))

filter(iris_sub, Sepal.Width>5,
       Species == "versicolor")

arrange(iris_sub, desc(Sepal.Length))

# Select
select(iris_sub, -Sepal.Width)

select(iris_sub, starts_with("Sepal"))

select(iris_sub, ends_with ("Width"))

# Mutate

x_max <- nrow(iris_sub)
x<-1:x_max

mutate(iris_sub, row_id = x)

mutate(iris_sub, sl_two_times = 2*Sepal.Length)

mutate(iris_sub, id_name = paste(Species, Sepal.Length))

mutate(iris_sub, iris_pw = paste(Petal.Width,Species))

iris_petal<-select(iris_sub, starts_with("Petal"))
print(iris_petal)

iris_pw_two<-mutate(iris_sub, pw_two_times = 2*Petal.Width)
print(iris_pw_two)

# Pipe %>% (control+ shift + m)

select (filter (iris_sub, Species == "virginica"), Sepal.Length)

df_vir_sl<-iris_sub %>%
  filter(Species=="virginica") %>%
  select (Sepal.Length)

print(df_vir_sl)


iris_pipe<-iris_sub |> 
  filter(Species == "setosa") |> 
  mutate(pw_two_times = 2*Petal.Width)

print(iris_pipe)

# Grouping
iris_sub |> 
  group_by(Species) |> 
  mutate(mu_s1 = mean(Sepal.Length)) |> 
  ungroup()

# Summarizing
iris_sub |> 
  group_by(Species) |> 
  summarize(mu_s1 = mean(Sepal.Length))


#' regression lab

rm(list = ls())
pacman::p_load(tidyverse)


# 12.4.1 Develop regression models
# using iris
# Sepal.Width: response
# Petal.Width: predictor

# perform by species manually
v_sp <- unique(iris$Species)

## v_sp[1] = setosa
fit1 <- iris %>% 
  filter(Species == v_sp[1]) %>% 
  lm(Sepal.Width ~ Petal.Width, data = .)

## v_sp[2] = versicolor
fit2 <- iris %>% 
  filter(Species == v_sp[2]) %>% 
  lm(Sepal.Width ~ Petal.Width, data = .)

## v_sp[3] = virginica
fit3 <- iris %>% 
  filter(Species == v_sp[3]) %>%
  lm(Sepal.Width ~ Petal.Width, data = .)

# perform at once using lapply() or `for` loop

## lapply version
list_lm <- lapply(X = v_sp,
                  function(s) {
                    iris %>% 
                      filter(Species == s) %>% 
                      lm(Sepal.Width ~ Petal.Width, data = .)
                  })

## for loop version
## `list` is the object type that can store multiple types of data as a single object
## for example, list[[1]] dataframe, list[[2]] vector, list[[3]] model output, etc...
## in list, accessing each element uses syntax [[]]
fit <- list(NULL)
for (i in 1:length(v_sp)) {
  fit[[i]] <- iris %>% 
    filter(Species == v_sp[i]) %>% 
    lm(Sepal.Width ~ Petal.Width, data = .)
}


# 12.4.2 Multiple explanatory variables
# additional predictor, Petal.Length by species
# compare R^2

# lm(y ~ x1 + x2, data = data) is the format for two variables case

# this is how I'd write if I don't use the dataframe again
fit1 <- iris %>% 
  filter(Species == v_sp[1]) %>% 
  lm(Sepal.Width ~ Petal.Width + Petal.Length, data = .)

# this is how I'd write if I will use the dataframe again
df_sp1 <- iris %>% 
  filter(Species == v_sp[1])
lm(Sepal.Width ~ Petal.Width + Petal.Length, data = df_sp1)

# lapply approach
list_lm_two <- lapply(
  X = v_sp,
  FUN = function(s) {
    iris %>% 
      filter(Species == s) %>% 
      lm(Sepal.Width ~ Petal.Width + Petal.Length, data = .)
  }
)

# when comparing R^2, see multiple R^2 vs adjusted R^2
# and look up the difference (in meaning) between multiple R^2 and adjusted R^2

iris_x <- iris %>% 
  mutate(useless = rnorm(150))

m0 <- lm(Sepal.Width ~ Petal.Width, data = iris_x)
m1 <- lm(Sepal.Width ~ Petal.Width + useless, data = iris_x)




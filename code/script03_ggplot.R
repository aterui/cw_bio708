
## call every single time!
library(tidyverse)


# point figure ------------------------------------------------------------

## basic point figure
iris %>% 
  ggplot(
    mapping = aes(x = Sepal.Length,
                  y = Sepal.Width)
  ) +
  geom_point()

## error, no aes()
## short cut for commenting out, Ctr + Shift + C
# iris %>% 
#   ggplot(
#     x = Sepal.Length,
#     y = Sepal.Width
#   ) +
#   geom_point()

## color by species
ggplot(
  data = iris,
  mapping = aes(x = Sepal.Length,
                y = Sepal.Width,
                color = Species)
) +
  geom_point()

ggplot(
  data = iris,
  mapping = aes(x = Sepal.Length,
                y = Sepal.Width)
) +
  geom_point(color = "steelblue")

## error, color arg outside aes()
# ggplot(
#   data = iris,
#   mapping = aes(x = Sepal.Length,
#                 y = Sepal.Width),
#   color = Species,
# ) +
#   geom_point()


# line figure -------------------------------------------------------------

df0 <- tibble(
  x = rep(1:50, 3),
  y = x * 2
)

df0 %>% 
  ggplot(
    mapping = aes(x = x,
                  y = y)
  ) +
  geom_line()


# histogram ---------------------------------------------------------------

iris %>% 
  ggplot(mapping = aes(x = Sepal.Length)) +
  geom_histogram()


# box plot ----------------------------------------------------------------

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

## fill arg changes the BOX color
iris %>% 
  ggplot(
    mapping = aes(x = Species,
                  y = Sepal.Length,
                  fill = Species)
  ) +
  geom_boxplot()


# fun plot ----------------------------------------------------------------

iris %>% 
  ggplot(aes(x = Sepal.Length,
             y = Sepal.Width,
             color = Species)) +
  geom_point() +
  geom_smooth(method = "lm")

iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Width)) +
  geom_boxplot(outliers = FALSE) +
  geom_jitter(width = 0.1,
              alpha = 0.1)

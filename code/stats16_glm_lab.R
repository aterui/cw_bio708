#' GLM Lab

pacman::p_load(tidyverse,
               patchwork)

# glm exercise ------------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

m_fish <- glm(n_sp ~ distance + cat_area + hull_area,
              data = df_fish,
              family = "poisson")

summary(m_fish)

m_am <- glm(am ~ mpg + hp + wt,
            data = mtcars,
            family = "binomial")

summary(m_am)

m_am_normal <- glm(am ~ mpg + hp + wt,
                   data = mtcars,
                   family = "gaussian")

summary(m_am_normal)

# effect size -------------------------------------------------------------

df_fish <- df_fish %>% 
  mutate(
    std_dist = c(scale(distance)),
    std_cat = c(scale(cat_area)),
    std_hull = c(scale(hull_area))
  )

## use scaled variables for glm
## compare output with glm with raw predictors without transformation

m_fish_std <- glm(n_sp ~ std_dist + std_cat + std_hull,
                  data = df_fish,
                  family = "poisson")

## comparison
summary(m_fish)
summary(m_fish_std)

# offset term -------------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_offset.csv"
df_offset <- read_csv(url) %>% 
  mutate(density = count / area)

## do visualization
g_area <- df_offset %>% 
  ggplot(
    aes(x = area,
        y = count)
  ) +
  geom_point()

g_nitrate <- df_offset %>% 
  ggplot(
    aes(x = nitrate,
        y = count)
  ) +
  geom_point()

g_den <- df_offset %>% 
  ggplot(
    aes(x = nitrate,
        y = density)
  ) +
  geom_point()

g_area | g_nitrate | g_den

## model with errors
# glm(density ~ nitrate,
#     data = df_offset,
#     family = "poisson")

## model with offset term
m_pois_off <- glm(count ~ nitrate + offset(log(area)),
                  data = df_offset,
                  family = "poisson")

m_normal <- glm(density ~ nitrate,
                data = df_offset,
                family = "gaussian")




# overdispersion ----------------------------------------------------------

## Poisson and Binomial distributions (and some other distributions)
## do not have a parameter that defines the variance independently.
## poisson: Y ~ Poisson(lambda); lambda is the sole parameter
##  - mean = lambda, variance = lambda

## binomial: Y ~ Binomial(N, p); p is the sole parameter (N is the data you supply)
##  - mean = N * p, variance = N * p * (1 - p)

## normal: Y ~ Normal(mu, sigma^2); sigma^2 is independent of mu and captures the variance in your data
##  - mean = mu, variance = sigma^2

## For Poisson and Binomial, once the mean is determined, the variance is also defined
## As such, there is an "expected" variance given the mean
## If the observed variance goes beyond the expected variance,
## that will result in "over-dispersion" and cause serious consequences for
## statistical inference

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_tadpole.csv"
df_tadpole <- read_csv(url)

m_tad <- glm(tadpole ~ aqveg + permanence,
             data = df_tadpole,
             family = "poisson")

df_tadpole %>% 
  ggplot(
    aes(x = aqveg,
        y = tadpole)
  ) +
  geom_point()

df_tadpole %>% 
  ggplot(
    aes(x = permanence,
        y = tadpole)
  ) +
  geom_point()

mean(df_tadpole$tadpole)
sd(df_tadpole$tadpole)
var(df_tadpole$tadpole)

## negative-binomial is better
MASS::glm.nb(tadpole ~ aqveg + permanence,
             data = df_tadpole) %>% 
  summary()


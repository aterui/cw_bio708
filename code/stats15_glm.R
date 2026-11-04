#' GLM

pacman::p_load(tidyverse,
               patchwork)


# poisson -----------------------------------------------------------------

## read data
df_count <- read_csv("data_src/data_garden_count.csv")
print(df_count)

## draw a figure relating nitrate to plant count
df_count %>% 
  ggplot(
    aes(x = nitrate,
        y = count)
  ) +
  geom_point()

## fit lm() to this data, and draw a predicted line
## use geom_abline() for prediction/fitted line
## y = count, x = nitrate
m_lm <- lm(count ~ nitrate, data = df_count)
df_count %>% 
  ggplot(
    aes(x = nitrate,
        y = count)
  ) +
  geom_point() +
  geom_abline(
    intercept = coef(m_lm)[1],
    slope = coef(m_lm)[2]
  )

## alternative to ordinary regression
## "Poisson" regression for count data
m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")

summary(m_pois)

## use intercept and slope as is to draw prediction
## geom_abline()
df_count %>% 
  ggplot(
    aes(x = nitrate,
        y = count)
  ) +
  geom_point() +
  geom_abline(
    intercept = coef(m_pois)[1],
    slope = coef(m_pois)[2]
  )

## link function, log(lambda) = alpha + beta * x  for Poisson
x <- seq(min(df_count$nitrate),
         max(df_count$nitrate),
         length = 100)

a <- coef(m_pois)[1] # intercept in a log-scale
b <- coef(m_pois)[2] # slope in a log-scale

log_y_pred <- a + b * x
y_pred <- exp(log_y_pred)

df_pred <- tibble(y_pred = y_pred,
                  nitrate = x)

df_count %>% 
  ggplot(
    aes(x = nitrate,
        y = count)
  ) +
  geom_point() +
  geom_line(
    data = df_pred,
    aes(y = y_pred)
  )

## compare results between lm() and glm()
## any difference?
summary(m_lm)
summary(m_pois)


# binomial ----------------------------------------------------------------

df_mussel <- read_csv("data_src/data_mussel.csv")

## draw figure
df_mussel %>%
  mutate(p = n_fertilized / n_examined) %>% 
  ggplot(
    aes(x = density,
        y = p)
  ) +
  geom_point()

## binomial regression
## n_examined is the possible maximum success
## n_fertilized is the success numbers
## binomial distributions takes two parameters
## Bin(N, p), where N is the number of trials, p is success probability
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")

summary(m_binom)

## draw prediction...?
z <- seq(min(df_mussel$density),
         max(df_mussel$density),
         length = 100)

theta <- coef(m_binom)
logit_p <- theta[1] + theta[2] * z
p_pred <- exp(logit_p) / (1 + exp(logit_p))

df_p <- tibble(p_pred = p_pred,
               density = z)

df_mussel %>% 
  mutate(p = n_fertilized / n_examined) %>% 
  ggplot(
    aes(x = density,
        y = p)
  ) +
  geom_point() +
  geom_line(
    data = df_p,
    aes(y = p_pred)
  )








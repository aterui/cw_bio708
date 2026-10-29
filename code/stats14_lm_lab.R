#' linear model, lab

rm(list = ls())
pacman::p_load(tidyverse,
               patchwork)


# normality assumption ----------------------------------------------------

df_tooth <- as_tibble(ToothGrowth)
m_tooth <- lm(len ~ supp * dose,
              data = df_tooth)

eps <- resid(m_tooth)
shapiro.test(eps)


# model interpretation ----------------------------------------------------

## data frame for prediction
df_pred <- ToothGrowth %>%
  group_by(supp) %>%
  reframe(dose = seq(min(dose),
                     max(dose),
                     length = 100))

## generate prediction
y_pred <- predict(m_tooth, newdata = df_pred)
df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

## figure
ToothGrowth %>% 
  ggplot(aes(
    x = dose,
    y = len,
    color = supp
  )) +
  geom_point(
    alpha = 0.2
  ) +
  geom_line(
    data = df_pred, # switch data frame!
    y = y_pred
  ) +
  theme_bw()


# multicolinearlity -------------------------------------------------------

## variance-covariance matrix
mv <- rbind(c(1, 0.9),
            c(0.9, 1))

## true regression coefficients
b <- c(0.05, 1.00)

## produce simulated data
set.seed(523)
X <- MASS::mvrnorm(100,
                   mu = c(0, 0),
                   Sigma = mv)

df_y <- tibble(x1 = X[,1],
               x2 = X[,2]) %>% 
  mutate(y = rnorm(nrow(.),
                   mean = 1 + b[1] * x1 + b[2] * x2))

## scatter plot
gx1 <- df_y %>% 
  ggplot(
    aes(
      x = x1,
      y = y
    )
  ) +
  geom_point()

gx2 <- df_y %>% 
  ggplot(
    aes(
      x = x2,
      y = y
    )
  ) +
  geom_point()

gx1 + gx2

## model analysis
lm(y ~ x1 + x2,
   data = df_y) %>% 
  summary()

## check predictor relationship
df_y %>% 
  ggplot(
    aes(
      x = x1,
      y = x2
      
    )
  ) +
  geom_point()

## correlation coef
cor(df_y$x1, df_y$x2)




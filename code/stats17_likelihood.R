#' Likelihood

pacman::p_load(tidyverse,
               patchwork)


# poisson example for likelihood ------------------------------------------

## probability of observing 3 given lambda = 3.5
(pr3 <- dpois(x = 3, lambda = 3.5))

## try 0.1 - 10 as possible values of lambda (intervals 0.1)
## find the best value of lambda that maximizes Pr(x = 3)
lambda <- seq(0.1, 10, by = 0.1)

df_pois <- tibble(lambda = lambda,
                  pr = dpois(3, lambda = lambda)) %>% 
  arrange(desc(pr))

## vector case y = {3, 2, 5}
## probability of observing 3 data points together
## use lambda = 3.5
y <- c(3, 2, 5)
pr <- dpois(x = y, lambda = 3.5)

# manual calculation
lik <- pr[1] * pr[2] * pr[3]

# product
lik <- prod(pr)

# likelihood for each lambda value
# prob of observing vector y
lambda <- seq(0.01, 10, by = 0.01)
lik_i <- sapply(lambda,
                function(x) {
                  prod(dpois(y, lambda = x))
                })

df_pois3 <- tibble(
  lambda = lambda,
  lik = lik_i
) %>% 
  arrange(desc(lik))

## sample mean of y
mean(y)

## why sample mean matches the MLE
## Pr(y = 3) with lambda = 3.5
dpois(3, lambda = 3.5)
(3.5^3 * exp(-3.5)) / factorial(3)

## visualization
df_pois3 %>% 
  ggplot(
    aes(x = lambda,
        y = log(lik))
  ) +
  geom_line() +
  labs(x = "lambda value",
       y = "Likelihood")

## get likelihood from glm
df_count <- read_csv("data_src/data_garden_count.csv")

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = poisson)

logLik(m_pois)

summary(m_pois)


#' Probability Distribution Lab

pacman::p_load(tidyverse,
               patchwork)


# normal distribution -----------------------------------------------------

# generate 50 observations
x <- rnorm(
  n = 50,
  mean = 10,
  sd = 2
)

# get sample mean and SD
mu <- mean(x)
sig <- sd(x)

# define bins
x_min <- floor(min(x))
x_max <- ceiling(max(x))

bin <- seq(
  x_min, 
  x_max, 
  by = 1
)

# calculate probability for each bin
p <- NULL
for (i in 1:(length(bin) - 1)) {
  xu <- pnorm(bin[i + 1], mean = mu, sd = sig)
  xl <- pnorm(bin[i], mean = mu, sd = sig)
  
  p[i] <- xu - xl
}

# get frequency - p * "50" because we have 50 observations
# or length(x) (safer)
df_prob <- tibble(
  p = p,
  bin = bin[-length(bin)] + 0.5
) %>% 
  mutate(
    freq = p * length(x)
  )

# draw histogram
df_x <- tibble(x = x)

df_x %>% 
  ggplot(aes(x = x)) +
  geom_histogram(
    binwidth = 1,
    center = 0.5
  ) +
  geom_point(
    data = df_prob, # switch data frame!
    aes(x = bin,
        y = freq),
    color = "tomato"
  ) +
  geom_line(
    data = df_prob,
    aes(x = bin,
        y = freq),
    color = "tomato",
    linetype = "dotted"
  )


# poisson distribution ----------------------------------------------------

# generate poisson-distributed random numbers
z <- rpois(
  n = 1000,
  lambda = 10
)

# sample mean
lambda <- mean(z)

# bins
bin <- seq(min(z), max(z), by = 1)

# probability (mass): dpois() returns "probability" (not prob density)
# because its representation is discrete (Pr(x = 1) = 0.2, etc.)
# continuous distributions (e.g., normal) return "probability density"
pm <- dpois(
  x = bin,
  lambda = lambda
)

# prepare data frames
df_z <- tibble(z = z)

df_prob <- tibble(
  pm = pm,
  bin = bin
) %>% 
  mutate(freq = pm * nrow(df_z))

# draw figures
df_z %>% 
  ggplot(
    aes(x = z)
  ) +
  geom_histogram(
    binwidth = 0.5,
    center = 0
  ) +
  geom_point(
    data = df_prob, # switch data frame
    aes(
      x = bin,
      y = freq
    ),
    color = "skyblue"
  ) +
  geom_line(
    data = df_prob, # switch data frame
    aes(
      x = bin,
      y = freq
    ),
    color = "skyblue",
    linetype = "dashed"
  )





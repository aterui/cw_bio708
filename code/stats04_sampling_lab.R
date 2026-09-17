#' Sampling Lab

rm(list = ls())
pacman::p_load(tidyverse,
               patchwork)


# exercise 1 --------------------------------------------------------------

## read csv file from source
df_h0 <- read_csv("data_src/data_plant_height.csv")

## initialize objects
mu_i_50 <- mu_i_100 <- var_i_50 <- var_i_100 <- NULL

for (i in 1:1000) {
  df_50 <- df_h0 %>% 
    sample_n(size = 50)
  
  df_100 <- df_h0 %>% 
    sample_n(size = 100)
  
  mu_i_50[i] <- mean(df_50$height)
  mu_i_100[i] <- mean(df_100$height)
  
  var_i_50[i] <- var(df_50$height)
  var_i_100[i] <- var(df_100$height)
}

df_sample <- tibble(mu50 = mu_i_50,
                    mu100 = mu_i_100,
                    var50 = var_i_50,
                    var100 = var_i_100)

g50mu <- df_sample %>% 
  ggplot(aes(x = mu50)) +
  geom_histogram() +
  scale_x_continuous(limits = c(10, 25))

g100mu <- df_sample %>% 
  ggplot(aes(x = mu100)) +
  geom_histogram() +
  scale_x_continuous(limits = c(10, 25))

g50var <- df_sample %>% 
  ggplot(aes(x = var50)) +
  geom_histogram() +
  scale_x_continuous(limits = c(10, 50))

g100var <- df_sample %>% 
  ggplot(aes(x = var100)) +
  geom_histogram() +
  scale_x_continuous(limits = c(10, 50))

(g50mu / g100mu) | (g50var / g100var)

## lapply() version
list_out <- lapply(X = c(50, 100),
                   FUN = function(x) {
                     
                     mu_i <- var_i <- NULL
                     for (i in 1:1000) {
                       df_i <- df_h0 %>% 
                         sample_n(size = x)
                       
                       mu_i[i] <- mean(df_i$height)
                       var_i[i] <- var(df_i$height)
                     }
                     
                     tibble(mu_hat = mu_i, 
                            var_hat = var_i,
                            n = x)
                   })



# exercise2 ---------------------------------------------------------------

df_h10 <- df_h0 %>% 
  filter(height >= 10)

mu_i <- var_i <- NULL

for(i in 1:1000) {
  
  df_i <- df_h10 %>% 
    sample_n(size = 50)
  
  mu_i[i] <- mean(df_i$height)
  var_i[i] <- var(df_i$height)
  
}

df_s10 <- tibble(mu_hat = mu_i,
                 var_hat = var_i)

g_mu_gr10 <- df_s10 %>% 
  ggplot(aes(x = mu_i)) +
  geom_histogram() +
  geom_vline(xintercept = mean(df_h0$height))

g_var_gr10 <- df_s10 %>% 
  ggplot(aes(x = var_i)) +
  geom_histogram() +
  geom_vline(xintercept = var(df_h0$height))

g_mu_gr10 + g_var_gr10


# extra -------------------------------------------------------------------

df_s10 %>% 
  pivot_longer(
    cols = everything(), 
    names_to = "measure",
    values_to = "value"
  ) %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(facets =~ measure)




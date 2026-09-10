#' Descriptive statistics - lab

library(tidyverse)

# central tendency --------------------------------------------------------

# Create a new vector z with length 100 as exp(rnorm(n = 100, mean = 0, sd = 0.1)), and calculate the arithmetic mean, geometric mean, and median of z.

z <- exp(rnorm(n = 100, mean = 0, sd = 0.1))
mu1 <- mean(z) # or sum(z) / length(z)
mu2 <- exp(mean(log(z))) # or prod(z)^(1 / length(z))
mu3 <- median(z)
  
# Draw a histogram of z using functions tibble(), ggplot(), and geom_histogram().

df_z <- tibble(z = z)

(g_hist <- df_z %>% 
  ggplot(aes(x = z)) +
  geom_histogram())

# Draw vertical lines of arithmetic mean, geometric mean, and median on the histogram with different colors using a function geom_vline() .

g_hist +
  geom_vline(xintercept = mu1,
             color = "black") +
  geom_vline(xintercept = mu2,
             color = "salmon") +
  geom_vline(xintercept = mu3,
             color = "skyblue")
  

# Visually compare the values of the central tendency measures with the vertical lines drawn by geom_vline().

## your answer

# Create a new vector z_rev as -z + max(z) + 0.5, and repeat step 1 – 4.

z_rev <- -z + max(z) + 0.5
mu1_r <- mean(z_rev)
mu2_r <- exp(mean(log(z_rev)))
mu3_r <- median(z_rev)

df_zr <- tibble(z_rev = z_rev)

(g_hist_zr <- df_zr %>% 
  ggplot(aes(x = z_rev)) +
  geom_histogram())

g_hist_zr + 
  geom_vline(xintercept = mu1_r) +
  geom_vline(xintercept = mu2_r,
             color = "salmon") +
  geom_vline(xintercept = mu3_r,
             color = "skyblue")


# variation ---------------------------------------------------------------

w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w

## Convert the unit of w to “milligram” and create a new vector m.

m <- 1000 * w

## Calculate SD and MAD for w and m.

(sd_w <- sqrt(sum((w - mean(w))^2) / length(w)))
(mad_w <- median(abs(w - median(w))))

(sd_m <- sqrt(sum((m - mean(m))^2) / length(m)))
(mad_m <- median(abs(m - median(m))))

## Calculate CV and MAD/Median for w and m.

## should be same!
sd_w / mean(w)
sd_m / mean(m)

mad_w / median(w)
mad_m / median(m)

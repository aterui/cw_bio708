#' more than two groups (ANOVA) - Lab

pacman::p_load(tidyverse)
rm(list = ls())

# The data set consists of two columns: weight, group. Create figures similar to Figure 11.1.
df_pg <- as_tibble(PlantGrowth)

df_pg %>% 
  mutate(
    group_label = case_when(
      group == "ctrl" ~ "Control",
      group == "trt1" ~ "Treatment 1",
      group == "trt2" ~ "Treatment 2"
    )
  ) %>% 
  ggplot(
    aes(
      x = group_label,
      y = weight
    )
  ) +
  geom_violin(
    draw_quantiles = 0.5,
    alpha = 0.2
  ) +
  geom_jitter(
    width = 0.2,
    alpha = 0.2
  ) +
  labs(
    x = "Treatment group",
    y = "Weight"
  ) +
  theme_classic()

# Conduct an ANOVA to examine whether there are differences in weight among the different group.

fit <- aov(weight ~ group, data = df_pg)
summary(fit)

# report values
# F value, p-value, df (sometimes, SS... but rare)

# Power analysis
pwr::pwr.anova.test(
  k = 3,
  f = 0.5,
  sig.level = 0.05,
  power = 0.8
)

## compare how the number of groups affects power
pwr::pwr.anova.test(
  k = 3,
  n = 10,
  f = 0.5,
  sig.level = 0.05
)

pwr::pwr.anova.test(
  k = 10,
  n = 3,
  f = 0.5,
  sig.level = 0.05
)

## compare how the number of samples per group affects power

## or any other question you may have!


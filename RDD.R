library(tidyverse)
library(haven)
library(fixest)
library(modelsummary)
library(patchwork)
library(rdd)
library(broom)
install.packages("purrr")
library(purrr)
rd_dt <- read_dta("data/mexico_localities.dta")

#Task1.1---------
rd_dt <- rd_dt |>
  mutate(pes_offer = ifelse(points >= 0, 1, 0))

rd_dt <- rd_dt |>
  group_by(state_id) |>
  mutate(recentered_mng_index = mng_index_raw - mean(mng_index_raw))

rd_dt |>
  ggplot(aes(x = points, y = lnIMLoc2010_EJawm)) +
  geom_point(size = 0.1) +
  stat_smooth(aes(group = pes_offer), method = "lm", se = FALSE) +
  labs(x = "Applicant score determining eligibility",
       y = "log poverty index measured in 2010") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  rd_dt |>
  ggplot(aes(x = points, y = recentered_mng_index)) +
  geom_point(size = 0.1) +
  stat_smooth(aes(group = pes_offer), method = "lm", se = FALSE) +
  labs(x = "Applicant score determining eligibility",
       y = "Land Management Index recentered to state average") +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red")


#Task1.2--------
rd_dt_mod <- rd_dt |>
  group_by(bin, cohort) |>
  summarise(sample_average = mean(recentered_mng_index),
            sample_size = n())
rd_dt |>
  ggplot(aes(x = points, y = recentered_mng_index)) +
  stat_smooth(
    aes(group = pes_offer),
    method = "lm",
    formula = y ~ poly(x, 2),
    se = FALSE,
    color = "darkblue"
  ) +
  geom_point(data = rd_dt_mod, aes(x = bin, y = sample_average, size = sample_size)) +
  stat_smooth(
    aes(group = pes_offer),
    method = "lm",
    se = FALSE,
    color = "lightgrey",
    alpha = 0.5
  ) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "red") +
  facet_wrap(~ cohort) +
  labs(
    x = "Eligibility Score (Running Variable)",
    y = "Management Index",
    size = "Sample size in bin",
    caption = "Blue line shows quadratic fit. Grey line shows linear fit"
  ) +
  theme(plot.caption = element_text(hjust = 0.5))


#Task2.1----------
h <- c(1:5)
h <- set_names(h)

dropped_rd_dt <- rd_dt |>
  filter(cohort == "2013-2014")

fun <- function(x) {
  lm(
    mng_index_raw ~ pes_offer + points + points:pes_offer + I(points ^ 2) +
      I(points ^ 2):pes_offer,
    data = dropped_rd_dt |>
      filter(points <= x & points >= -x)
  )
}
bw_RD <- map(h, fun)

modelsummary(
  bw_RD,
  estimate = "estimate",
  statistic = "std.error",
  coef_omit = c(-2),
  gof_map = "nobs"
)

alt <- function(x) {
  lm(
    mng_index_raw ~ pes_offer + points + points:pes_offer + I(points ^ 2) +
      I(points ^ 2):pes_offer,
    data = dropped_rd_dt |>
      filter(points <= x & points >= -x)
  ) |>
    tidy()
}

bw_RD1 <- purrr::map(h, alt)
summary_table1 <- list_rbind(bw_RD1, names_to = "Bandwidth")

summary_table1 |>
  filter(term == "pes_offer") |>
  ggplot(aes(x = Bandwidth, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(
    ymin = estimate - 1.96 * std.error,
    ymax = estimate + 1.96 *
      std.error
  ),
  width = 0) +
  geom_hline(yintercept = 0, color = "red") +
  labs(y = "PES Treatment Effect",
       title = "Treatment effects at cutoff under various bandwidths")

#Task2.2-----------

rdd::RDestimate(
  mng_index_raw ~ points + pes_offer + points:pes_offer + I(points ^ 2) +
    I(points ^ 2):pes_offer,
  dropped_rd_dt,
  kernel = "triangular"
) |>
  summary()

rdd::RDestimate(
  mng_index_raw ~ points + pes_offer + points:pes_offer + I(points ^ 2)
  + I(points ^ 2):pes_offer,
  dropped_rd_dt,
  kernel = "rectangular"
) |>
  summary()

#Task2.3--------

rd_dt |>
  group_by(pes_offer) |>
  summarise(control_mean = mean(mng_index_raw),
            control_sd = sd(mng_index_raw))

#Task3-----------

feols(mng_index_raw ~ points + points:pes_offer | uptake ~ pes_offer,
      dropped_rd_dt) |>
  summary()


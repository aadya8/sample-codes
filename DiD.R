library(tidyverse)
library(haven)
library(fixest)
library(modelsummary)

#Task1.1----
mss <- read_dta("data/miguel_africa.dta")
fit_regular <- lm(any_prio ~ gdp_g_l + polity2l, mss)
fit_iv <- feols(any_prio ~ polity2l | gdp_g_l ~ GPCP_g_l, mss)
modelsummary(
  list(OLS = fit_regular,
       IV = fit_iv),
  coef_map = c(
    `(Intercept)` = "Intercept",
    gdp_g_l = "Lagged Economic Growth",
    polity2l = "Democracy(Polity IV)",
    fit_gdp_g_l = "Lagged Economic Growth"
  ),
  gof_map = c("nobs")
)

#Task2.1-----
ss <- read_dta("data/ScheveStasavage_repdata.dta")
ss |>
  ggplot(aes(x = year, y = topitaxrate2, group = country)) +
  annotate(
    "rect",
    xmin = 1915,
    xmax = 1918,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.5,
    fill = "skyblue"
  ) +
  annotate(
    "rect",
    xmin = 1941,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.5,
    fill = "skyblue"
  ) +
  geom_line(linewidth = 0.2) +
  theme_classic() +
  labs(y = "Top Marginal Tax Rate(%)", x = "Year")

#Task2.2----
plot <-
  ggplot(ss, aes(x = year, y = topitaxrate2, group = country)) +
  geom_line() +
  geom_line(
    data = select(ss,-country),
    aes(group = ccode),
    alpha = 0.2,
    linewidth = 0.2
  ) +
  facet_wrap( ~ country, nrow = 4) +
  annotate(
    "rect",
    xmin = 1915,
    xmax = 1918,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.7,
    fill = "skyblue"
  ) +
  annotate(
    "rect",
    xmin = 1941,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.7,
    fill = "skyblue"
  ) +
  theme_classic() +
  labs(y = "Top Marginal Tax Rate(%)", x = "Year")
ggsave("ss_timetrend.png", w = 9, h = 4)

#Task3.2-----
sub_ss <- ss |>
  filter(country %in% c("UK", "Netherlands"), !year %in% c(1915:1948)) |>
  mutate(
    pre_post = case_when(year <= 1914 ~ "Pre-war",
                         year >= 1949 ~ "Post-war"),
    pre_post = fct_relevel(pre_post, "Pre-war")
  )

tibble <- sub_ss |>
  group_by(pre_post) |>
  summarise(UK = mean(topitaxrate2[country == "UK"]),
            Netherlands = mean(topitaxrate2[country == "Netherlands"]))

#Task3.3-------
fit_did <- lm(topitaxrate2 ~ pre_post * country, sub_ss)
summary(fit_did)

library(tidyverse)
library(haven)
library(modelsummary)
library(panelView)
library(fixest)
library(broom)
library(glue)
library(flextable)
library(scales)

panel_raw <- read_dta("data/ScheveStasavage_annual.dta")

war_status <- panelView::panelview(
  topitax ~ war2p,
  data = panel_raw,
  index = c("country", "year"),
  xlab = "Year",
  ylab = "Country",
  axis.lab.gap = c(10, 0),
  cex.lab = 9.5,
  main = "Treatment Status of War Mobilisation"
)
suffrage_status <- panelView::panelview(
  topitax ~ unisuffrage,
  data = panel_raw,
  index = c("country", "year"),
  xlab = "Year",
  ylab = "Country",
  axis.lab.gap = c(10, 0),
  cex.lab = 9.5,
  main = "Treatment Status of Universal Suffrage"
)
compt_election_status <- panelView::panelview(
  topitax ~ democracy,
  data = panel_raw,
  index = c("country", "year"),
  xlab = "Year",
  ylab = "Country",
  axis.lab.gap = c(10, 0),
  cex.lab = 9.5,
  main = "Treatment Status of Competitive Elections"
)
#Task2.1---------------------------------------------------
five_panel <- read_dta("data/ScheveStasavage_5year.dta")

five_panel <- five_panel |>
  mutate(trend_JPN = ifelse(country == "Japan", cumsum(country == "Japan"), 0))

five_panel <- five_panel |>
  mutate(trend_NZ = ifelse(country == "New Zealand", cumsum(country == "New Zealand"), 0))

#Task2.2---------------------------------------------------
five_panel <- five_panel |>
  group_by(country) |>
  mutate(
    war_lagged = lag(war2p, 1),
    unisuffrage_lagged = lag(unisuffrage, 1),
    leftexec_lagged = lag(leftexec, 1),
    rgdppc_lagged = lag(rgdppc, 1),
    topitax_lagged = lag(topitax, 1)
  ) |>
  arrange(year)

trend_colnames <- str_subset(colnames(five_panel), "trend_")
coef_names <-
  "war_lagged + unisuffrage_lagged + leftexec_lagged + rgdppc_lagged"

col1 <-
  feols(
    topitax ~ war_lagged + unisuffrage_lagged |
      country + year,
    five_panel,
    ssc = ssc(fixef.K = "full")
  )
spec_form2 <- glue("topitax ~ {coef_names} | country + year")

col2 <-
  feols(as.formula(spec_form2), five_panel, ssc = ssc(fixef.K = "full"))

spec_form <-
  glue("topitax ~ {coef_names} + {str_c(trend_colnames, collapse = '+')} | country + year")
col3 <-
  feols(as.formula(spec_form), five_panel, ssc = ssc(fixef.K = "full"))

modelsummary(
  list(
    Without_control = col1,
    With_control = col2,
    With_control_linear_time_trend = col3
  ),
  coef_omit = "trend_",
  gof_omit = "AIC|BIC|RMSE|Within"
)

#Task3----------------------------------------
five_panel <- five_panel |>
  group_by(country) |>
  mutate(
    war_lagged2 = lag(war2p, 2),
    war_lead = lead(war2p, 1),
    war_lead2 = lead(war2p, 2),
    war_lagged3 = lag(war2p, 3),
    war_lead3 = lead(war2p, 3)
  ) |>
  arrange(year)
lag_lead <-
  grep("(_lagged2$|_lagged3$|_lead$|_lead2$|_lead3$)",
       colnames(five_panel),
       value = TRUE)
spec_form3 <-
  glue(
    "topitax ~ {coef_names} + {str_c(lag_lead, collapse = '+')} + {str_c(trend_colnames, collapse = '+')}| country + year"
  )
lagged_lead_fit <- feols(as.formula(spec_form3), five_panel) |>
  tidy()

lagged_lead_fit |>
  mutate(
    term = recode_factor(
      term,
      war_lagged3 = "t-3",
      war_lagged2 = "t-2",
      war_lagged = "t-1",
      war_lead = "t+1",
      war_lead2 = "t+2",
      war_lead3 = "t+3",
      .default = NA_character_
    )
  ) |>
  filter(!is.na(term)) |>
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 *
                      std.error),
                width = 0) +
  geom_hline(yintercept = 0, color = "red") +
  labs(x = "Effect of war mobilisation with upto 3-term leads and 3-term lags",
       y = "Top Marginal Income Tax Rate")


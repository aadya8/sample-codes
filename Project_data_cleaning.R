library(tidyverse)
library(haven)
library(gt)
library(fixest)
library(glue)
library(broom)
library(readxl)
library(modelsummary)

allison <- read_dta("data/Final_Main.dta")
supplemental <- read_dta("data/Final_Supplemental2.dta")
amnesty <- read_dta("data/amnesty.dta")
natural_resource <-
  read_excel("data/finalwdi_natural_resource.xlsx")
regional_classification <-
  read_excel("data/regional_classification.xlsx")

allison <- allison |>
  filter(year > 1986)

amnesty_mod <- amnesty |>
  filter(year > 1986) |>
  select(year, ccode, bdeadbest)

mod <- supplemental |>
  select(year, ccode, demoaid, multiaid, l1multiaid, l1demoaid)

allison2 <- allison |>
  left_join(amnesty_mod, by = c("ccode", "year")) |>
  left_join(natural_resource, by = c("ccode", "year")) |>
  left_join(mod, by = c("ccode", "year")) |>
  mutate(
    lag_battledeaths = dplyr::lag(bdeadbest, 2),
    lag_natural_res = dplyr::lag(wdi_indicator, 2),
    .by = ccode
  )

allison_mod <- allison2 |>
  left_join(regional_classification, by = "ISO")

#Summary stats---------------------------------------------------------------------------

summary <- allison2 |>
  select(
    starts_with("cov") &
      !ends_with("F"),
    new_empinx,
    polity2,
    ends_with("avg"),
    EV,
    l2CPcol2,
    bdeadbest,
    lag_natural_res,
    multiaid,
    demoaid
  ) |>
  pivot_longer(cols = everything()) |>
  group_by(name) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    n = sum(!is.na(value))
  )

summary <-
  summary |> #deal with summary$name as unless summary is run, code for str_subset doesn't run
  mutate(
    category = case_when(
      name == "new_empinx" ~ "Outcome:Human Rights Characteristics",
      name == "new_empinxavg" ~ "Outcome:Human Rights Characteristics",
      name == "polity2" ~ "Outcome:Democracy characteristics",
      name == "polity2avg" ~ "Outcome:Democracy characteristics",
      name == "EV" ~ "Treatment:Foreign Aid Characteristics",
      name == "l2CPcol2" ~ "Instrument:Colonial status characteristics",
      .default = "Covariates"
    )
  ) |>
  group_by(category)

summary |>
  mutate(
    name = recode_factor(
      name,
      EV = "Logged Net EU Aid (OECD), time t-1, from ODA in Millions of 1995 Constant U.S. Dollars +1",
      l2CPcol2 = "Colony Status, time (t-2)2",
      new_empinx = "Human Empowerment Index (CIRI, score 0 to 14)",
      new_empinxavg = "Human Empowerment Index (averaged over time t to t+3)",
      polity2 = "Democracy level (Polity IV, score -10 to 10)",
      polity2avg = "Democracy level (averaged over time t to t+3)",
      covdemregion = "Democracies in Region (Source: Polity IV)",
      coviNY_GDP_PETR_RT_ZS = "Petroleum Revenues (Source: WDI)",
      covihme_ayem = "Average Years Education in Male (Source: Gakidou et al 2010)",
      covloggdp = "Log(GDP) (Source: WDI)",
      covloggdpC = "Log(GDP per capita) (Source:WDI)",
      covwdi_exp = "Log(Exports) (Source: WDI)",
      covwdi_fdi = "FDI (Source:WDI)",
      covwdi_imp = "Log(Imports) (Source:WDI)",
      covwvs_rel = "Religiosity",
      bdeadbest = "Number of Battledeaths by Country-Year (Source: Lacina & Gleditsch 2005)",
      lag_natural_res = "Total Natural Resource Rents (as % of GDP) (Source: WDI)",
      multiaid = "Logged Multilateral Aid (Source: WDI)",
      demoaid = "Logged Democracy aid (Source: WDI)",
    )
  ) |>
  arrange(name) |>
  gt(groupname_col = "category") |>
  cols_align("left", 1) |>
  cols_label(mean = "Mean", sd = "Std. Dev", name = "") |>
  fmt_number(columns = c(mean, sd), decimals = 2) |>
  fmt_integer(columns = n) |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_row_groups()) |>
  gtsave(filename = "summary_table.png", expand = 3)


#Replication-----------------------------------------------------------------------

covariates <- str_subset(colnames(allison), "cov")
coef1 <-
  glue("new_empinxavg ~ {str_c(covariates, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
coef7 <-
  glue("polity2avg ~ {str_c(covariates, collapse = ' + ')} | year + ccode | EV ~ l2CPcol2")
fit_test <-
  feols(as.formula(coef1), data = allison, vcov = "twoway")
fitstat(fit_test, "ivf")
fit_test2 <-
  feols(as.formula(coef7), data = allison, vcov = "twoway")
fitstat(fit_test2, "ivf")

fit2 <-
  feols(new_empinxavg ~ 1 |
          year + ccode | EV ~ l2CPcol2,
        data = allison,
        vcov = "twoway")
fit_test3 <-
  feols(polity2avg ~ 1 |
          year + ccode | EV ~ l2CPcol2,
        data = allison,
        vcov = "twoway")
fitstat(fit2, ~ ivf)

summary_row <- tribble(
  ~ term,
  ~ "CIRI Human Empowerment Index",
  ~ "CIRI Human Empowerment Index",
  ~ "Polity IV Combined Score",
  ~ "Polity IV Combined Score",
  "Countries",
  "115",
  "115",
  "95",
  "95",
  "Years",
  "20",
  "20",
  "20",
  "20",
  "Covariates",
  "Yes",
  "No",
  "Yes",
  "No",
  "Year Fixed Effects",
  "Yes",
  "Yes",
  "Yes",
  "Yes",
  "Country Fixed Effects",
  "Yes",
  "Yes",
  "Yes",
  "Yes",
  "N",
  "1792",
  "1792",
  "1818",
  "1818"
)

modelsummary(
  list(
    `(1)` = fit_test,
    `(2)` = fit2,
    `(3)` = fit_test2,
    `(4)` = fit_test3
  ),
  add_rows = summary_row,
  coef_omit = "cov",
  gof_omit = "R2|AIC|BIC|RMSE|Std.Errors|FE|Num.Obs.",
  coef_rename = c("fit_EV" = "Effect of Aid"),
  title = "Two-Stage Least Squares Estimates of Effects of Logged Foreign Aid (in Year t−1) from the European Community on Dependent Variables Averaged over Years t through t + 3",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  notes = c(
    "Note:In column 1, the following covariates are not shown: Average Years Education, Log Exports, FDI, Log Imports, Religiosity, Petroleum Revenues, Democracies in Region, Log GDP, and Log GDP per Capita. Dummies indicating missing values are also not shown. Fixed effects held for country and year. Robust standard errors (accounting for two-way clustering at the levels of country and year) are shown in parentheses.
            First-stage coefficient absent covariates on Colonyi(t−2)2 for CIRI regression is 0.160 (SE = 0.046, p = .002, F = 5.26). First-stage coefficient on Colonyi(t−2)2 for Polity IV Combined Score regression absent covariates is 0.170 (SE = 0.052, p = .004, F = 6.31)"
  ),
  output = "gt"
) |>
  tab_spanner(label = "CIRI Human Empowerment Index", columns = 2:3) |>
  tab_spanner(label = "Polity IV Combined Score", columns = 4:5) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_spanners(),
      cells_column_labels(),
      cells_title()
    )
  ) |>
  cols_width(everything() ~ px(150)) |>
  cols_align(align = "center", columns = -1)  |>
  gtsave(filename = "main_table.png", expand = 5)


#authors inclusion of multiaid and demoaid for new_empinxavg-------------------------------

coef2 <-
  glue(
    "new_empinxavg ~ {str_c(covariates, collapse = ' + ')} + l1demoaid + l1multiaid | year + ccode | EV ~ l2CPcol2"
  )
other_aid <- feols(as.formula(coef2), data = allison_mod)

other_aid_wc <-
  feols(
    new_empinxavg ~ l1demoaid + l1multiaid |
      year + ccode | EV ~ l2CPcol2,
    data = allison2,
    vcov = "twoway"
  )
#this is what authors did to prove that receiving other kinds of aid doesn't reduce the effect of EU's aid on score

coef10 <-
  glue(
    "polity2avg ~ {str_c(covariates, collapse = ' + ')} + demoaid + multiaid | year + ccode | EV ~ l2CPcol2"
  )
polity_other_aid <- feols(as.formula(coef10), data = allison_mod)

polity_other_aid_simple <-
  feols(polity2avg ~ demoaid + multiaid |
          year + ccode | EV ~ l2CPcol2, data = allison2)
#led to author's conclusion that other sources of aid doesn't diminish the impact of foreign aid

summary_row1 <- tribble(
  ~ term,
  ~ "(1)",
  ~ "(2)",
  ~ "(3)",
  ~ "(4)",
  "Countries",
  "115",
  "115",
  "95",
  "95",
  "Years",
  "20",
  "20",
  "20",
  "20",
  "Covariates",
  "No",
  "Yes",
  "No",
  "Yes",
  "Year Fixed Effects",
  "Yes",
  "Yes",
  "Yes",
  "Yes",
  "Country Fixed Effects",
  "Yes",
  "Yes",
  "Yes",
  "Yes",
  "N",
  "1792",
  "1792",
  "1818",
  "1818"
)
modelsummary(
  list(
    `(1)` = other_aid_wc,
    `(2)` = other_aid,
    `(3)` = polity_other_aid_simple,
    `(4)` = polity_other_aid
  ),
  add_rows = summary_row1,
  coef_omit = "cov",
  gof_omit = "R2|AIC|BIC|RMSE|Std.Errors|FE|Num.Obs.",
  title = "Author's Approach to Control for Other Aid",
  output = "gt"
) |>
  tab_spanner(label = "CIRI Human Empowerment Index", columns = 2:3) |>
  tab_spanner(label = "Polity IV Combined Score", columns = 4:5) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_spanners(),
      cells_column_labels(),
      cells_title()
    )
  ) |>
  cols_align(align = "center", columns = -1) |>
  gtsave(filename = "appendix_other_aid.png")

#battledeaths - prio dataset - limited sample size left after cleaning, hence rejected -----------------------------------

battledeaths <- read_excel("data/prio_battledeaths.xls")

battledeaths <- battledeaths |>
  separate_rows(gwnoloc, sep = ",") |>
  mutate(ccode = as.numeric(gwnoloc)) |>
  mutate(
    dummy_conflict_war = ifelse(int == 2, 1, 0),
    dummy_conflict_minor = ifelse(int == 1, 1, 0)
  ) |>
  select(year,
         ccode,
         type,
         dummy_conflict_war,
         dummy_conflict_minor,
         int,
         location)

battledeaths <- battledeaths |>
  filter(year > 1986)

allison1 <- allison |>
  left_join(battledeaths,
            by = c("year", "ccode"),
            unmatched = "drop") |>
  left_join(natural_resource, by = c("year", "ccode")) |>
  mutate(
    lag_war = dplyr::lag(dummy_conflict_war, 2),
    lag_minor = dplyr::lag(dummy_conflict_minor, 2),
    lag_natural_res = dplyr::lag(wdi_indicator, 2),
    .by = ccode
  )


coef3 <-
  glue(
    "new_empinxavg ~ {str_c(covariates, collapse = ' + ')} + lag_war + lag_minor + lag_natural_res | year + ccode | EV ~ l2CPcol2"
  )
only_conflict <- feols(as.formula(coef3), data = allison1)
#negative coefficient


#testing model with new covariates---------------

#on human rights score
coef4 <-
  glue(
    "new_empinxavg ~ {str_c(covariates, collapse = ' + ')} + lag_battledeaths + lag_natural_res + l1demoaid + l1multiaid | year + ccode | EV ~ l2CPcol2"
  )
coef8 <-
  glue(
    "new_empinxavg ~ {str_c(covariates, collapse = ' + ')} + lag_battledeaths + lag_natural_res | year + ccode | EV ~ l2CPcol2"
  )
coef9 <-
  glue(
    "new_empinxavg ~ {str_c(covariates, collapse = ' + ')} + lag_natural_res + l1demoaid + l1multiaid | year + ccode | EV ~ l2CPcol2"
  )
only_battledeaths <- feols(as.formula(coef4), data = allison_mod)
partial_battledeaths <- feols(as.formula(coef8), data = allison_mod)
without_battledeaths <- feols(as.formula(coef9), data = allison_mod)


#on polity2 score
coef5 <-
  glue(
    "polity2avg ~ {str_c(covariates, collapse = ' + ')} + lag_battledeaths + lag_natural_res + l1demoaid + l1multiaid | year + ccode | EV ~ l2CPcol2"
  )
coef6 <-
  glue(
    "polity2avg ~ {str_c(covariates, collapse = ' + ')} + lag_battledeaths + lag_natural_res | year + ccode | EV ~ l2CPcol2"
  ) #no need to show this separately
coef20 <-
  glue(
    "polity2avg ~ {str_c(covariates, collapse = ' + ')} + lag_natural_res + l1demoaid + l1multiaid | year + ccode | EV ~ l2CPcol2"
  )
for_polity <- feols(as.formula(coef5), data = allison_mod)
partial_polity <- feols(as.formula(coef6), data = allison_mod)
without_battledeaths2 <-
  feols(as.formula(coef20), data = allison_mod)
summary(without_battledeaths2)

summary_row3 <- tribble(
  ~ term,
  ~ "Simple IV + FE",
  ~ "Original covariates",
  ~ "New covariates with battledeaths",
  ~ "New covariates without battledeaths",
  ~ "Simple IV + FE",
  ~ "Original covariates",
  ~ "New covariates with battledeaths",
  ~ "New covariates without battledeaths",
  "Carnegie & Marinov Covariates",
  "No",
  "Yes",
  "Yes",
  "Yes",
  "No",
  "Yes",
  "Yes",
  "Yes"
)

summary_row2 <- tribble(
  ~ term,
  ~ "Simple IV + FE",
  ~ "Original covariates",
  ~ "New covariates with battledeaths",
  ~ "New covariates without battledeaths",
  ~ "Simple IV + FE",
  ~ "Original covariates",
  ~ "New covariates with battledeaths",
  ~ "New covariates without battledeaths",
  "Carnegie & Marinov Covariates",
  "No",
  "Yes",
  "Yes",
  "Yes",
  "No",
  "Yes",
  "Yes",
  "Yes",
  "Countries",
  "95",
  "95",
  "90",
  "91",
  "115",
  "115",
  "87",
  "106",
  "Years",
  "20",
  "20",
  "14",
  "18",
  "20",
  "20",
  "14",
  "18",
  "N",
  "1818",
  "1818",
  "1195",
  "1546",
  "1792",
  "1792",
  "1120",
  "1544"
)
summary(for_polity, stage = 1)
modelsummary(
  list(
    `Simple IV + FE` = fit_test3,
    `Original covariates` = fit_test2,
    `New covariates with battledeaths` = for_polity,
    `New covariates without battledeaths` = without_battledeaths2,
    `Simple IV + FE` = fit2,
    `Original covariates` = fit_test,
    `New covariates with battledeaths` = only_battledeaths,
    `New covariates without battledeaths` = without_battledeaths
  ),
  add_rows = summary_row2,
  gof_omit = "R2|AIC|BIC|RMSE|Std.Errors|FE|Num.Obs.",
  coef_omit = "cov",
  coef_rename = c(
    "fit_EV" = "Effect of Aid",
    "lag_battledeaths" = "Battledeaths",
    "lag_natural_res" = "Natural Resource Rent (% GDP)",
    "l1demoaid" = "Democratic aid",
    "l1multiaid" = "Multilateral aid"
  ),
  output = "gt"
) |>
  tab_spanner(label = "Polity IV Combined Score", columns = 2:5) |>
  tab_spanner(label = "CIRI Human Empowerment Index", columns = 6:9) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_spanners(),
      cells_column_labels(),
      cells_title()
    )
  ) |>
  cols_align(align = "center", columns = -1) |>
  gtsave(filename = "extension_table.png")



#regional classification for heterogeneity--------
allison_mod <- allison2 |>
  left_join(regional_classification, by = "ISO") #repeat code
europe <- allison_mod |>
  filter(regions == "Europe & Central Asia")
middle_east <- allison_mod |>
  filter(regions == "Middle East & North Africa")
sub_saharan_africa <- allison_mod |>
  filter(regions == "Sub-Saharan Africa")
south_asia <- allison_mod |>
  filter(regions == "South Asia")
east_asia <- allison_mod |>
  filter(regions == "East Asia & Pacific")
asia_combined <- allison_mod |>
  filter(regions == "South Asia" | regions == "East Asia & Pacific")
latin_america <- allison_mod |>
  filter(regions == "Latin America & Caribbean")

#heterogeneity effect on human rights score

asia_combined_fit <- feols(as.formula(coef4), data = asia_combined)

south_asia_fit <- feols(as.formula(coef4), data = south_asia)

middle_east_fit <- feols(as.formula(coef4), data = middle_east)

sub_saharan_africa_fit <-
  feols(as.formula(coef4), data = sub_saharan_africa)

latin_america_fit <- feols(as.formula(coef4), data = latin_america)

east_asia_fit <- feols(as.formula(coef4), data = east_asia)

latin_america_fit_author <-
  feols(as.formula(coef7), data = latin_america)

latin_america_fit_author2 <-
  feols(as.formula(coef1), data = latin_america)

-----------------------------------------------------------------------#heterogeneity effect on polity score
  asia_combined_fit1 <- feols(as.formula(coef5), data = asia_combined)

latin_america_fit1 <- feols(as.formula(coef5), data = latin_america)

east_asia_fit1 <- feols(as.formula(coef5), data = east_asia)

south_asia_fit1 <- feols(as.formula(coef5), data = south_asia)

sub_saharan_africa_fit1 <-
  feols(as.formula(coef5), data = sub_saharan_africa)

middle_east_fit1 <- feols(as.formula(coef5), data = middle_east)

#modelsummary for latin america------

summary_row4 <- tribble(
  ~ term,
  ~ "Carnegie & Marinov",
  ~ "New Covariates",
  ~ "Polity IV Combined Score",
  ~ "New Covariates",
  "C & M Covariates included",
  "Yes",
  "Yes",
  "Yes",
  "Yes",
  "Years",
  "20",
  "14",
  "20",
  "14",
  "Countries",
  "31",
  "14",
  "23",
  "23",
  "Year Fixed Effects",
  "Yes",
  "Yes",
  "Yes",
  "Yes",
  "Country Fixed Effects",
  "Yes",
  "Yes",
  "Yes",
  "Yes",
  "N",
  "485",
  "314",
  "441",
  "303"
)
modelsummary(
  list(
    `Carnegie & Marinov` = latin_america_fit_author2,
    `New Covariates` = latin_america_fit,
    `Carnegie & Marinov` = latin_america_fit_author,
    `New Covariates` = latin_america_fit1
  ),
  add_rows = summary_row4,
  gof_omit = "R2|AIC|BIC|RMSE|Std.Errors|FE|Num.Obs.",
  coef_omit = "cov",
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_rename = c(
    "fit_EV" = "Effect of Aid",
    "lag_battledeaths" = "Battledeaths",
    "lag_natural_res" = "Natural Resource Rent (% GDP)",
    "l1demoaid" = "Democratic aid",
    "l1multiaid" = "Multilateral aid"
  ),
  output = "gt"
) |>
  tab_spanner("CIRI Human Empowerment Index", columns = 2:3) |>
  tab_spanner("Polity IV Combined Score", columns = 4:5) |>
  gtsave(filename = "Latin_America.png")

#figure for effect on unaveraged scores

allison2 <- allison2 |>
  mutate(
    lag_polity1 = dplyr::lead(polity2, 1),
    lag_polity2 = dplyr::lead(polity2, 2),
    lag_polity3 = dplyr::lead(polity2, 3),
    lag_polity4 = dplyr::lead(polity2, 4),
    lag_polity5 = dplyr::lead(polity2, 5),
    .by = ccode
  )

coef11 <- glue("polity2 ~ 1 | year + ccode | EV ~ l2CPcol2")
coef12 <- glue("lag_polity1 ~ 1 | year + ccode | EV ~ l2CPcol2")
coef13 <- glue("lag_polity2 ~ 1 | year + ccode | EV ~ l2CPcol2")
coef14 <- glue("lag_polity3 ~ 1 | year + ccode | EV ~ l2CPcol2")
coef15 <- glue("lag_polity4 ~ 1 | year + ccode | EV ~ l2CPcol2")
coef16 <- glue("lag_polity5 ~ 1 | year + ccode | EV ~ l2CPcol2")
fit11 <- feols(as.formula(coef11), data = allison2)
fit12 <- feols(as.formula(coef12), data = allison2)
fit13 <- feols(as.formula(coef13), data = allison2)
fit14 <- feols(as.formula(coef14), data = allison2)
fit15 <- feols(as.formula(coef15), data = allison2)
fit16 <- feols(as.formula(coef16), data = allison2)

#effects from t to t+5

library(plotrix)
library(gtools)

quartz("", 8, 4)

par(mfrow = c(1, 2),
    family = "Times",
    mar = c(4.75, 4.1, 3, 1))

point = 15

# Empowerment
coefs <-
  c(2.711  ,      1.494     ,   0.908  ,      0.390   ,     0.527     ,  0.0848)
ses <-
  c(1.349    ,    1.140      ,  0.450  ,      0.741   ,     0.831     ,   0.562)

plotCI(
  x = c(0:(length(coefs) - 1)),
  y = coefs,
  uiw = 1.96 * ses,
  xlab = "Years Forward",
  ylab = "Effect of Foreign Aid",
  ylim = c(-6, 6),
  pch = point,
  xaxp = c(0, length(coefs) - 1, (length(coefs) - 1)),
  scol = "#999999",
  main = "CIRI Human Empowerment Index"
)
lines(y = c(0, 0), x = c(-100, 100))


# Polity Score
coefs <-
  c(1.398  ,      1.384  ,      2.323    ,    2.504    ,    2.018     ,   1.326)
ses <-
  c(2.333      ,  0.926     ,   0.765 ,       0.484   ,     0.935    ,    1.233)

Polity <-
  plotCI(
    x = c(0:(length(coefs) - 1)),
    y = coefs,
    uiw = 1.96 * ses,
    xlab = "Years Forward",
    ylab = "Effect of Foreign Aid",
    ylim = c(-10, 10),
    pch = point,
    xaxp = c(0, length(coefs) - 1, (length(coefs) - 1)),
    scol = "#999999",
    main = "Polity IV Score"
  )
lines(y = c(0, 0), x = c(-100, 100))

-----------------------------------------------------------------------------------------------#summary of aid compositions and scores across regions------
allison_mod |>
  arrange(ccode, year) |>
  summarise(
    natural_resource_rents = mean(wdi_indicator, na.rm = TRUE),
    aid_composition = mean(EV, na.rm = TRUE),
    polity2_avg = mean(polity2avg, na.rm = TRUE),
    empinx_avg = mean(new_empinxavg, na.rm = TRUE),
    .by = regions
  )

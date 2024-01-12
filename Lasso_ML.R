library(tidyverse)
library(rsample)
library(glmnet)
library(glmnetUtils)
library(glue)
library(gt)

osha <- read.csv("data/osha.csv")
#Task1.1-------
injury_r <- lm(osha$injury_rate ~ osha$estab_age_netsmm1 + osha$exp_imp_netsmm1 + osha$exporter_netsmm1 +
                    osha$foreign_owned_netsmm1 + osha$govt_contractor_netsmm1 + osha$headquarters_netsmm1 + 
                    osha$importer_netsmm1 + osha$public_netsmm1 + osha$standalone_netsmm1 + osha$num_yrs_prev_on_sst, osha)
summary(injury_r)$r.squared
predicted <- predict(injury_r)
SSR <- sum((osha$injury_r - predicted)^2)
TSS <- sum((osha$injury_r - mean(osha$injury_r))^2)
r_squared <- 1 - (SSR/TSS)

#Task1.2------
mod_osha <- osha |>
  select(-c(id, injuries, injury_rate, high_injury_rate)) |>
  colnames()
spec_form <- glue("injury_rate ~ {str_c(mod_osha, collapse = '+')}")
sink_model <- lm(as.formula(spec_form), osha)

form_simpl <- injury_rate ~ has_tmin1_odi + any_insp_prior + 
  any_complaint_tmin13 + num_nonfat_comp_insp_cy_tc99mm1 + 
  initial_pen_cy_mzmm1 + ln_initial_pen_cy_mzmm1
simple_model <- lm(form_simpl, osha)
tribble(~simple_model, ~sink_model, ~workplace_type_model,
        summary(simple_model)$r.squared, summary(sink_model)$r.squared,
        summary(injury_r)$r.squared)

#Task2.1-------
set.seed(06510)
split <- initial_split(osha, prop = 0.8)
osha_train <- training(split)
osha_test <- testing(split)

#Task2.2------
lasso_model <- cv.glmnet(as.formula(spec_form), osha_train)
sink_model1 <- lm(as.formula(spec_form), osha_train)
simple_model1 <- lm(form_simpl, osha_train)

osha_test <- osha_test |>
  mutate(
    pred_lasso = as.vector(predict(lasso_model, osha_test, s = "lambda.min")),
    pred_sink = as.vector(predict(sink_model1, osha_test)),
    pred_simple = as.vector(predict(simple_model1, osha_test))
  ) 
osha_train <- osha_train |>
  mutate(
    pred_lasso = as.vector(predict(lasso_model, osha_train, s = "lambda.min")),
    pred_sink = as.vector(predict(sink_model1, osha_train)),
    pred_simple = as.vector(predict(simple_model1, osha_train))
  )

df_combined <- bind_rows(osha_test |>
                           mutate(set = "Test Dataset"),
                         osha_train |>
                           mutate(set = "Training Dataset"))
df_long <- df_combined |>
  pivot_longer(cols = c(pred_lasso,pred_sink,pred_simple),
               names_to = "model",
               values_to = "predicted_outcome")

table <- df_long |>
  select(set, model, predicted_outcome, injury_rate) |>
  group_by(set, model) |>
  summarise(RMSE = sqrt(mean((injury_rate - predicted_outcome)^2))) |>
  pivot_wider(
    id_cols = model,
    names_from = set,
    values_from = RMSE
  )

table$model <- c("Lasso Model","Simple Model","Sink Model")
gt(table) |>
  tab_header(title = "RMSE")

#task2.3------
lasso_coef <- enframe(as.vector(coef(lasso_model, s = "lambda.min")))
sum(lasso_coef$value != 0)

#Task3----
fs2 <- update(form_simpl, high_injury_rate ~.)
sf2 <- update(as.formula(spec_form), high_injury_rate ~.)

logit_simple <- glm(fs2, osha_train, family = "binomial")
logit_sink <- glm(sf2, osha_train, family = "binomial")
logit_lasso <- cv.glmnet(sf2, osha_train, family = "binomial")

osha_test <- osha_test |>
  mutate(pred_logit_simple = predict(logit_simple, osha_test, type = "response"),
         pred_logit_sink = predict(logit_sink, osha_test, type = "response"),
         pred_logit_lasso = predict(logit_lasso, osha_test, s = "lambda.min", type = "response"))

osha_test |>
  mutate(high_injury_rate = as.factor(high_injury_rate)) |>
  ggplot(aes(x = pred_logit_sink, y = pred_logit_lasso, color = high_injury_rate, shape = high_injury_rate)) +
  geom_point() +
  scale_color_manual(labels = c("No/0","Yes/1"),
                    values = c("gray", "red")) +
  scale_shape_manual(labels = c("No/0","Yes/1"),
                     values = c(20,17)) +
  labs(color = "High Injury Case",
       shape = "High Injury Case",
       x = "Predicted probability(high injury) by kitchen sink logit model",
       y = "Predicted probability(high injury) by lasso kitchen sink logit model")

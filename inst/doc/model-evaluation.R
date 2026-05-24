## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  warning = FALSE,
  message = FALSE,
  error = TRUE
)

## ----setup--------------------------------------------------------------------
library(clinpubr)
library(dplyr)
library(survival)
library(ggplot2)

## ----prepare-data-------------------------------------------------------------
data(cancer, package = "survival")
cancer$dead <- cancer$status == 2
cancer$event <- ifelse(cancer$status == 2, 1, 0)

cancer_clean <- cancer %>%
  mutate(sex = factor(sex, labels = c("Male", "Female"))) %>%
  na.omit()

# Fit three models with increasing complexity
cancer_clean$pred_model1 <- predict(
  glm(dead ~ age + sex + ph.karno, data = cancer_clean, family = binomial),
  type = "response"
)
cancer_clean$pred_model2 <- predict(
  glm(dead ~ age + sex + ph.karno + pat.karno, data = cancer_clean, family = binomial),
  type = "response"
)
cancer_clean$pred_model3 <- predict(
  glm(dead ~ age + sex + ph.karno + wt.loss, data = cancer_clean, family = binomial),
  type = "response"
)

knitr::kable(head(cancer_clean[, c("dead", "pred_model1", "pred_model2", "pred_model3")]),
  caption = "Model Predictions vs Actual Outcomes (First 6 Rows)"
)

## ----model-compare------------------------------------------------------------
model_comparison <- classif_model_compare(
  data = cancer_clean,
  target_var = "dead",
  model_names = c("pred_model1", "pred_model2", "pred_model3"),
  save_output = FALSE
)

knitr::kable(model_comparison$metric_table, caption = "Model Performance Comparison")

## ----model-plots-roc----------------------------------------------------------
model_comparison$roc_plot

## ----model-plots-pr-----------------------------------------------------------
model_comparison$pr_plot

## ----model-plots-calibration--------------------------------------------------
model_comparison$calibration_plot

## ----model-plots-dca----------------------------------------------------------
model_comparison$dca_plot

## ----fit-survival-model-------------------------------------------------------
# Fit Cox model for survival prediction
cox_model <- coxph(Surv(time, event) ~ age + sex + ph.karno, data = cancer_clean)
cancer_clean$risk_score <- predict(cox_model, type = "risk")

## ----time-roc-----------------------------------------------------------------
time_roc_result <- time_roc_plot(
  data = cancer_clean,
  event_var = "event",
  time_var = "time",
  marker_var = "risk_score",
  times = c(200, 365),
  time_unit = "days",
  save_plot = FALSE
)

time_roc_result

## ----c-index------------------------------------------------------------------
c_index <- calc_cindex(cancer_clean, "time", "event", "risk_score")
c_index

## ----importance-plot----------------------------------------------------------
importance_scores <- c(
  Age = 0.25, Sex = 0.15, Karnofsky = 0.30,
  WeightLoss = 0.12, Calories = 0.08, PatKarnofsky = 0.10
)

importance_plot(x = importance_scores, x_lab = "Variable Importance")

## ----custom-importance--------------------------------------------------------
importance_plot(
  x = importance_scores,
  x_lab = "Relative Importance",
  top_n = 4,
  color = "steelblue"
)


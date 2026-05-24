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

## ----load-data----------------------------------------------------------------
data(cancer, package = "survival")
cancer$dead <- cancer$status == 2

cancer_clean <- cancer %>%
  mutate(
    sex = factor(sex, labels = c("Male", "Female"))
  ) %>%
  na.omit()

knitr::kable(data.frame(
  Rows = nrow(cancer_clean),
  Columns = ncol(cancer_clean),
  Description = "Complete cases for analysis"
), caption = "Analysis Dataset Summary")

knitr::kable(head(cancer_clean), caption = "Sample of Analysis Variables")

## ----cox-scan-----------------------------------------------------------------
cox_scan <- regression_scan(
  data = cancer_clean,
  y = "status",
  time = "time",
  # set predictors = NULL to scan all available variables
  # predictors = NULL,
  predictors = c("age", "sex", "ph.karno", "pat.karno", "wt.loss", "meal.cal"),
  covars = NULL,
  save_table = FALSE
)

knitr::kable(cox_scan, caption = "Cox Regression Scan Results")

## ----logistic-scan------------------------------------------------------------
logistic_scan <- regression_scan(
  data = cancer_clean,
  y = "dead",
  predictors = c("age", "sex", "ph.karno", "pat.karno", "wt.loss"),
  covars = NULL,
  save_table = FALSE
)

knitr::kable(logistic_scan, caption = "Logistic Regression Scan Results")

## ----cox-basic-results--------------------------------------------------------
cox_results <- regression_basic_results(
  data = cancer_clean,
  x = "age",
  y = "status",
  time = "time",
  model_covs = list(
    Crude = c(),
    Model1 = c("sex"),
    Model2 = c("sex", "ph.karno", "pat.karno")
  ),
  pers = c(1, 5, 10),
  save_output = FALSE
)

knitr::kable(cox_results$table, caption = "Cox Regression Results for Age")

## ----logistic-basic-results---------------------------------------------------
logistic_results <- regression_basic_results(
  data = cancer_clean,
  x = "ph.karno",
  y = "dead",
  model_covs = list(
    Crude = c(),
    Adjusted = c("age", "sex")
  ),
  factor_breaks = c(80),
  factor_labels = c("<80", ">=80"),
  save_output = FALSE
)

knitr::kable(logistic_results$table, caption = "Logistic Regression Results for Performance Status")

## ----custom-transformations---------------------------------------------------
custom_results <- regression_basic_results(
  data = cancer_clean,
  x = "age",
  y = "status",
  time = "time",
  quantile_breaks = c(0.33, 0.67),
  quantile_labels = c("Youngest Third", "Middle Third", "Oldest Third"),
  label_with_range = TRUE,
  save_output = FALSE
)

knitr::kable(custom_results$table, caption = "Cox Regression Results - Age Tertiles")

## ----univariate-forest--------------------------------------------------------
uni_forest <- regression_forest(
  data = cancer_clean,
  model_vars = c("age", "ph.karno", "pat.karno", "wt.loss"),
  y = "status",
  time = "time",
  as_univariate = TRUE, # Generate univariate forest plot
  save_plot = FALSE
)

uni_forest

## ----multivariate-forest------------------------------------------------------
multi_forest <- regression_forest(
  data = cancer_clean,
  model_vars = list(
    Model1 = c("age"),
    Model2 = c("age", "ph.karno"),
    Model3 = c("age", "ph.karno", "pat.karno")
  ),
  y = "status",
  time = "time",
  as_univariate = FALSE, # Generate multivariate forest plot
  save_plot = FALSE
)

multi_forest

## ----custom-forest------------------------------------------------------------
custom_forest <- regression_forest(
  data = cancer_clean,
  model_vars = c("age", "ph.karno", "pat.karno", "wt.loss", "meal.cal"),
  y = "status",
  time = "time",
  as_univariate = TRUE,
  show_vars = c("age", "ph.karno", "pat.karno"),
  est_nsmall = 2,
  p_nsmall = 4,
  save_plot = FALSE
)

custom_forest

## ----predictor-effect---------------------------------------------------------
effect_linear <- predictor_effect_plot(
  data = cancer_clean,
  x = "age",
  y = "status",
  time = "time",
  covars = c("sex"),
  method = "linear",
  add_hist = TRUE,
  save_plot = FALSE
)

effect_linear

## ----predictor-effect-categorical---------------------------------------------
effect_cat <- predictor_effect_plot(
  data = cancer_clean,
  x = "sex",
  y = "status",
  time = "time",
  covars = c("age", "ph.karno"),
  method = "categorical",
  save_plot = FALSE
)

effect_cat

## ----rcs-plot-----------------------------------------------------------------
rcs_age <- rcs_plot(
  data = cancer_clean,
  x = "age",
  y = "status",
  time = "time",
  covars = c("sex", "ph.karno"),
  knot = 4,
  ref = "x_median",
  add_hist = TRUE,
  save_plot = FALSE
)

rcs_age

## ----custom-effect------------------------------------------------------------
custom_rcs <- rcs_plot(
  data = cancer_clean,
  x = "wt.loss",
  y = "status",
  time = "time",
  covars = c("age", "sex"),
  knot = 5,
  ref = 0,
  add_hist = FALSE,
  trans = "log10",
  y_lim = c(0.5, 5),
  save_plot = FALSE
)

custom_rcs

## ----interaction-scan---------------------------------------------------------
interaction_scan <- interaction_scan(
  data = cancer_clean,
  y = "status",
  time = "time",
  predictors = c("age", "ph.karno", "pat.karno"),
  group_vars = c("sex", "ph.karno"), # continuous variables will be split by median into subgroups
  save_table = FALSE
)

knitr::kable(interaction_scan, caption = "Interaction Scan Results")

## ----interaction-plot---------------------------------------------------------
interaction_age_sex <- interaction_plot(
  data = cancer_clean,
  predictor = "age",
  y = "status",
  time = "time",
  group_var = "sex",
  covars = c("ph.karno"),
  save_plot = FALSE
)

interaction_age_sex

## ----subgroup-forest----------------------------------------------------------
subgroup_result <- subgroup_forest(
  data = cancer_clean,
  x = "age",
  y = "status",
  time = "time",
  subgroup_vars = c("sex", "pat.karno"),
  covars = c("ph.karno"),
  save_plot = FALSE
)

subgroup_result


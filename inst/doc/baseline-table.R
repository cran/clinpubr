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

## ----load-data----------------------------------------------------------------
data(cancer, package = "survival")
str(cancer)
knitr::kable(head(cancer), caption = "Raw Data Preview")

## ----prepare-data-------------------------------------------------------------
cancer$age_group <- cut(cancer$age,
  breaks = c(0, 50, 60, 70, 100),
  labels = c("<50", "50-60", "60-70", ">70")
)

# Combine sparse ECOG categories
cancer$ph.ecog_cat <- factor(cancer$ph.ecog,
  levels = c(0:3),
  labels = c("0", "1", ">=2", ">=2")
)

# Add missing values for demonstration
set.seed(123)
cancer$meal.cal[sample(1:nrow(cancer), 30)] <- NA
cancer$wt.loss[sample(1:nrow(cancer), 20)] <- NA

knitr::kable(head(cancer), caption = "Data After Preparation")

## ----get-var-types------------------------------------------------------------
var_types <- get_var_types(cancer, strata = "sex")

var_types

## ----customize-var-types------------------------------------------------------
var_types_custom <- get_var_types(
  cancer,
  strata = "sex",
  num_to_factor = 10, # Numeric vars with <=10 unique values treated as factor
  omit_factor_above = 15, # Omit factors with >15 levels
  norm_test_by_group = TRUE # Test normality within each stratum
)

var_types_custom

## ----save-qqplots, eval = FALSE-----------------------------------------------
# # var_types_with_plots <- get_var_types(
# #   cancer, strata = "sex",
# #   save_qqplots = TRUE, folder_name = "qqplots_review"
# # )

## ----basic-baseline-----------------------------------------------------------
baseline_result <- baseline_table(
  cancer,
  var_types = var_types,
  save_table = FALSE
)

knitr::kable(baseline_result$baseline, caption = "Baseline Characteristics by Sex")

## ----multi-group--------------------------------------------------------------
data(cancer, package = "survival")
cancer$ph.ecog_cat <- factor(cancer$ph.ecog,
  levels = c(0:3),
  labels = c("0", "1", ">=2", ">=2")
)

var_types_ecog <- get_var_types(cancer, strata = "ph.ecog_cat")

baseline_multi <- baseline_table(
  cancer,
  var_types = var_types_ecog,
  save_table = FALSE,
  multiple_comparison_test = TRUE,
  p_adjust_method = "BH"
)

knitr::kable(baseline_multi$baseline, caption = "Baseline Characteristics by ECOG Status")
knitr::kable(baseline_multi$pairwise, caption = "Pairwise Comparison P-values")

## ----customize-baseline-------------------------------------------------------
baseline_custom <- baseline_table(
  cancer,
  var_types = var_types,
  vars = c("age", "wt.loss", "meal.cal", "ph.ecog"),
  smd = TRUE,
  omit_missing_strata = TRUE,
  seed = 123
)

knitr::kable(baseline_custom$baseline, caption = "Customized Baseline Table")

## ----missing-table------------------------------------------------------------
knitr::kable(baseline_result$missing, caption = "Missing Data Summary")

## ----manual-override----------------------------------------------------------
baseline_manual <- baseline_table(
  cancer,
  strata = "sex",
  factor_vars = c("ph.ecog", "pat.karno"),
  nonnormal_vars = c("age"),
  exact_vars = c("ph.ecog")
)

knitr::kable(baseline_manual$baseline, caption = "Baseline Table with Manual Overrides")

## ----save-results, eval = FALSE-----------------------------------------------
# # baseline_saved <- baseline_table(
# #   cancer, var_types = var_types,
# #   save_table = TRUE, filename = "baseline_characteristics.csv"
# # )

## ----complete-workflow--------------------------------------------------------
# Step 1: Prepare data
data(cancer, package = "survival")
cancer_clean <- cancer %>%
  mutate(
    age_group = cut(age,
      breaks = c(0, 50, 60, 70, 100),
      labels = c("<50", "50-60", "60-70", ">70")
    ),
    ph.ecog_cat = factor(ph.ecog,
      levels = c(0:3),
      labels = c("0", "1", ">=2", ">=2")
    ),
    sex = factor(sex, labels = c("Male", "Female"))
  )

# Step 2: Determine variable types
var_types <- get_var_types(cancer_clean, strata = "sex", num_to_factor = 5)

# Step 3: Review classification
knitr::kable(data.frame(
  Variable_Type = c("Factor", "Non-normal", "Exact"),
  Variables = c(
    paste(var_types$factor_vars, collapse = ", "),
    paste(var_types$nonnormal_vars, collapse = ", "),
    paste(var_types$exact_vars, collapse = ", ")
  )
), caption = "Variable Type Review")

# Step 4: Create baseline table
baseline_final <- baseline_table(
  cancer_clean,
  var_types = var_types,
  smd = TRUE
)

# Step 5: Review results
knitr::kable(baseline_final$baseline, caption = "Final Baseline Characteristics Table")
knitr::kable(baseline_final$missing, caption = "Final Missing Data Summary")


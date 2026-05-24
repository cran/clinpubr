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

## ----cohort-screening---------------------------------------------------------
set.seed(123)

patients <- data.frame(
  patient_id = 1:100,
  age = round(rnorm(100, 60, 15)),
  gender = sample(c("M", "F"), 100, replace = TRUE),
  has_baseline_data = sample(c(TRUE, FALSE), 100, prob = c(0.9, 0.1), replace = TRUE),
  meets_inclusion = sample(c(TRUE, FALSE), 100, prob = c(0.8, 0.2), replace = TRUE),
  complete_followup = sample(c(TRUE, FALSE), 100, prob = c(0.85, 0.15), replace = TRUE)
)

exclusion_summary <- exclusion_count(
  patients,
  age < 50,
  gender != "M",
  !has_baseline_data,
  !meets_inclusion,
  !complete_followup
)

knitr::kable(exclusion_summary, caption = "CONSORT-Style Cohort Flow")

cat("Retention rate:", round(100 * exclusion_summary$N[nrow(exclusion_summary)] / exclusion_summary$N[1], 1), "%\n")

## ----exclusion-custom---------------------------------------------------------
exclusion_flow <- exclusion_count(
  patients,
  age < 18 | age > 80,
  !has_baseline_data,
  !meets_inclusion,
  .criteria_names = c(
    "Age outside 18-80 range",
    "Missing baseline data",
    "Does not meet inclusion criteria"
  )
)

knitr::kable(exclusion_flow, caption = "Custom Exclusion Flow")

## ----merge-substring----------------------------------------------------------
medical_terms <- data.frame(
  match_term = c(
    "Type 2 Diabetes", "Hypertension", "Coronary Artery Disease",
    "Coronary Disease", "Chronic Kidney Disease", "Heart Failure", "Atrial Fibrillation"
  ),
  standard_term = c(
    "Type 2 Diabetes Mellitus", "Hypertension", "Coronary Artery Disease",
    "Coronary Artery Disease", "Chronic Kidney Disease", "Heart Failure", "Atrial Fibrillation"
  ),
  icd_code = c("E11", "I10", "I25", "I25", "N18", "I50", "I48"),
  category = c(
    "Endocrine", "Cardiovascular", "Cardiovascular", "Cardiovascular",
    "Renal", "Cardiovascular", "Cardiovascular"
  )
)

patient_diagnoses <- data.frame(
  patient_id = 1:10,
  diagnosis_text = c(
    "Severe Type 2 Diabetes", "Type 2 Diabetes Mellitus",
    "Type 2 Diabetes with Complications", "Essential Hypertension",
    "Hypertensive disease", "CAD - Coronary Artery Disease",
    "Coronary disease", "CKD Stage 3",
    "Congestive Heart Failure", "Heart failure chronic"
  )
)

merged_substring <- merge_by_substring(
  data = patient_diagnoses,
  key_df = medical_terms,
  search_col = "diagnosis_text",
  key_col = "match_term",
  value_cols = c("standard_term", "icd_code", "category")
)

knitr::kable(medical_terms, caption = "Medical Terms Table")
knitr::kable(merged_substring, caption = "Diagnoses with Mapped ICD Codes")

## ----merge-range--------------------------------------------------------------
patient_visits <- data.frame(
  patient_id = rep(1:3, each = 2),
  visit_id = 1:6,
  visit_start = as.Date(c(
    "2023-01-01", "2023-06-01", "2023-02-01",
    "2023-07-01", "2023-03-01", "2023-08-01"
  )),
  visit_end = as.Date(c(
    "2023-01-10", "2023-06-10", "2023-02-10",
    "2023-07-10", "2023-03-10", "2023-08-10"
  ))
)

lab_results <- data.frame(
  lab_id = 1:6,
  patient_id = c(1, 1, 2, 2, 3, 3),
  test_date = as.Date(c(
    "2023-01-05", "2023-06-05", "2023-02-03",
    "2023-07-08", "2023-03-15", "2023-08-05"
  )),
  test_name = c("Glucose", "HbA1c", "Glucose", "Creatinine", "Glucose", "Lipid panel"),
  result = round(rnorm(6, 100, 15), 1)
)

merged_range <- merge_by_range(
  x = patient_visits, y = lab_results,
  by = "patient_id",
  x_start = "visit_start", x_end = "visit_end",
  y_val = "test_date"
)

knitr::kable(merged_range, caption = "Labs Matched to Visit Windows")

## ----to-wide------------------------------------------------------------------
long_labs <- data.frame(
  patient_id = rep(1:5, each = 3),
  visit = rep(c(1, 2), times = c(8, 7)),
  test = rep(c("glucose", "creatinine", "cholesterol"), 5),
  value = round(rnorm(15, 100, 20), 1)
)

wide_labs <- to_wide(
  df = long_labs,
  keys = c("patient_id", "visit"),
  item_col = "test",
  value_col = "value"
)

knitr::kable(head(long_labs), caption = "Laboratory Data - Long Format")
knitr::kable(head(wide_labs), caption = "Laboratory Data - Wide Format")

## ----multichoice--------------------------------------------------------------
set.seed(456)
survey_data <- data.frame(
  id = 1:20,
  symptoms = sapply(1:20, function(x) {
    paste(sample(
      c("fever", "cough", "headache", "fatigue"),
      sample(1:4, 1)
    ), collapse = ",")
  }),
  comorbidities = sapply(1:20, function(x) {
    paste(sample(
      c("diabetes", "hypertension"),
      sample(1:2, 1)
    ), collapse = ",")
  })
)

symptoms_split <- split_multichoice(
  survey_data,
  quest_cols = c("symptoms", "comorbidities"),
  split = ",",
  remove_space = FALSE
)

knitr::kable(head(survey_data), caption = "Multi-Choice Data")

knitr::kable(head(symptoms_split), caption = "Split Multi-Choice Data")

combined <- combine_multichoice(
  symptoms_split,
  quest_cols = list(
    respiratory = c("symptoms_cough", "symptoms_fatigue"),
    systemic = c("symptoms_fever", "symptoms_headache")
  ),
  sep = ","
)

knitr::kable(head(combined), caption = "Combined Symptom Groups")

## ----string-utils-prefix------------------------------------------------------
file_names <- c("patient_001_lab.csv", "patient_001_visit.csv", "patient_001_demo.csv")
common_prefix(file_names)

## ----string-utils-replace-----------------------------------------------------
test_names <- c("Glucose_Fasting", "Glucose_Random", "Cholesterol_Total", "LDL_Calculated")
standardized <- str_match_replace(
  x = test_names,
  to_match = c("Glucose", "Cholesterol", "LDL"),
  to_replace = c("GLU", "CHOL", "LDL_CHOL")
)

knitr::kable(data.frame(Original = test_names, Standardized = standardized),
  caption = "Test Name Standardization"
)

## ----list-ops-add-------------------------------------------------------------
list1 <- list(diabetes = 10, hypertension = 20, asthma = 5)
list2 <- list(diabetes = 15, hypertension = 25, asthma = 8)
add_lists(list1, list2)

## ----list-ops-merge-----------------------------------------------------------
sites <- list(
  c("Diabetes", "Hypertension", "Heart Failure", "CKD"),
  c("Hypertension", "COPD", "Diabetes", "Cancer"),
  c("CKD", "Diabetes", "Stroke", "Hypertension")
)
merge_ordered_vectors(sites)

## ----list-ops-replace---------------------------------------------------------
sample_data <- data.frame(
  group = c("Cntrol", "Treetment", "Placebo", "Cntrol", "Treatment"),
  value = 1:5
)
recoded <- replace_elements(
  x = sample_data$group,
  from = c("Cntrol", "Treetment"),
  to = c("Control", "Treatment")
)
knitr::kable(data.frame(Original = sample_data$group, Recoded = recoded),
  caption = "Group Recoding"
)

## ----list-ops-fill------------------------------------------------------------
time_series <- c(120, NA, NA, 125, NA, 130, NA, NA)
knitr::kable(data.frame(Original = time_series, Filled = fill_with_last(time_series)),
  caption = "Last Observation Carried Forward"
)

## ----workflow-setup-----------------------------------------------------------
# Patient demographics
patient <- data.frame(
  pid = 1:50,
  age = sample(25:75, 50, replace = TRUE),
  gender = sample(c("M", "F"), 50, replace = TRUE)
)

# Admission records
admission <- data.frame(
  pid = c(1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  vid = 101:115,
  admit_day = c(1, 30, 5, 45, 10, 60, 15, 20, 25, 35, 40, 50, 55, 65, 70)
)

# Diagnosis records
diagnosis <- data.frame(
  pid = c(1, 1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  vid = c(101, 102, 103, 104, 105, 107, 108, 109, 110, 111, 112, 113, 114, 115),
  dx_day = c(1, 30, 5, 45, 10, 15, 20, 25, 35, 40, 50, 55, 65, 70),
  icd = c("E11", "I10", "N18", "E11", "E11", "I25", "E11", "J18", "E11", "I10", "E11", "N18", "E11", "I50")
)

# Lab results
lab <- data.frame(
  pid = c(1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  vid = c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115),
  lab_day = c(1, 30, 5, 45, 10, 60, 15, 20, 25, 35, 40, 50, 55, 65, 70),
  HbA1c = c(8.5, 7.2, 9.1, 7.8, 8.0, 6.9, 7.5, 8.2, 7.0, 9.5, 7.8, 8.1, 7.3, 8.8, 7.6)
)

## ----workflow-screen----------------------------------------------------------
screened_data <- screen_data_list(
  data_list = list(
    patient = patient, admission = admission,
    diagnosis = diagnosis, lab = lab
  ),
  entry_expr = any(icd == "E11"),
  entry_level = "patient_id",
  anchor_expr = icd == "E11",
  anchor_level = "date",
  patient_id_map = "pid",
  visit_id_map = c(admission = "vid", diagnosis = "vid", lab = "vid"),
  date_map = c(admission = "admit_day", diagnosis = "dx_day", lab = "lab_day"),
  output = "list",
  return_audit = TRUE
)

knitr::kable(screened_data$audit$entry_scope, caption = "Entry Stage Audit")
knitr::kable(screened_data$audit$anchor_scope, caption = "Anchor Stage Audit")

## ----workflow-review----------------------------------------------------------
knitr::kable(head(screened_data$data$patient), caption = "Filtered Patients")
knitr::kable(screened_data$data$diagnosis, caption = "Filtered Diagnoses")
knitr::kable(screened_data$data$lab, caption = "Filtered Labs")

## ----workflow-joined----------------------------------------------------------
joined_result <- screen_data_list(
  data_list = list(
    patient = patient, admission = admission,
    diagnosis = diagnosis, lab = lab
  ),
  entry_expr = any(icd == "E11"),
  entry_level = "patient_id",
  anchor_expr = any(icd == "E11"),
  anchor_level = "date",
  anchor_window = "from_first_anchor",
  patient_id_map = "pid",
  visit_id_map = c(admission = "vid", diagnosis = "vid", lab = "vid"),
  date_map = c(admission = "admit_day", diagnosis = "dx_day", lab = "lab_day"),
  output = "joined"
)

cat("Joined data:", nrow(joined_result), "rows,", ncol(joined_result), "columns\n")
knitr::kable(head(joined_result), caption = "Joined Output")


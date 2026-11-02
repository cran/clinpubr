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

## ----create-messy-data--------------------------------------------------------
set.seed(123)
n_patients <- 60

# Patient IDs and number of visits per patient (longitudinal cohort)
visits_per_patient <- sample(1:4, n_patients, replace = TRUE, prob = c(0.25, 0.4, 0.25, 0.1))
patient_ids <- rep(paste0("P", sprintf("%03d", 1:n_patients)), visits_per_patient)
n_visits <- length(patient_ids)

# Helper: generate a mixture of clinically plausible values
make_vals <- function(n, props, means, sds) {
  grp <- sample(seq_along(means), n, replace = TRUE, prob = props)
  round(rnorm(n, means[grp], sds[grp]), 2)
}

# Glucose: mix of mg/dL and mmol/L (1 mmol/L = 18 mg/dL)
glucose_units <- sample(c("mg/dL", "mmol/L", NA), n_visits,
  replace = TRUE, prob = c(0.6, 0.3, 0.1)
)
glucose_vals <- make_vals(
  n_visits, c(0.55, 0.25, 0.15, 0.05),
  c(100, 150, 230, 5.7), c(12, 18, 30, 0.9) # mg/dL values
)
mmol_glu_idx <- !is.na(glucose_units) & glucose_units == "mmol/L"
glucose_vals[mmol_glu_idx] <- make_vals(
  sum(mmol_glu_idx), c(0.55, 0.25, 0.15, 0.05),
  c(5.6, 8.3, 12.8, 0.32), c(0.7, 1, 1.7, 0.05) # mmol/L values
)

# Creatinine: mix of mg/dL and umol/L (1 mg/dL = 88.4 umol/L)
creatinine_units <- sample(c("mg/dL", "umol/L", NA), n_visits,
  replace = TRUE, prob = c(0.55, 0.35, 0.1)
)
creatinine_vals <- make_vals(
  n_visits, c(0.5, 0.25, 0.25),
  c(0.86, 1.7, 4.5), c(0.15, 0.3, 1.2) # mg/dL values
)
umol_cre_idx <- !is.na(creatinine_units) & creatinine_units == "umol/L"
creatinine_vals[umol_cre_idx] <- make_vals(
  sum(umol_cre_idx), c(0.5, 0.25, 0.25),
  c(76, 150, 400), c(13, 27, 110) # umol/L values
)

# Cholesterol: mix of mg/dL and mmol/L (1 mmol/L = 38.67 mg/dL)
cholesterol_units <- sample(c("mg/dL", "mmol/L", NA), n_visits,
  replace = TRUE, prob = c(0.6, 0.3, 0.1)
)
cholesterol_vals <- make_vals(
  n_visits, c(0.55, 0.3, 0.15),
  c(180, 230, 330), c(25, 20, 40) # mg/dL values
)
mmol_chol_idx <- !is.na(cholesterol_units) & cholesterol_units == "mmol/L"
cholesterol_vals[mmol_chol_idx] <- make_vals(
  sum(mmol_chol_idx), c(0.55, 0.3, 0.15),
  c(4.7, 6, 8.5), c(0.6, 0.5, 1) # mmol/L values
)

# A modest amount of missing values in each test
glucose_vals[sample(n_visits, round(n_visits * 0.08))] <- NA
creatinine_vals[sample(n_visits, round(n_visits * 0.08))] <- NA
cholesterol_vals[sample(n_visits, round(n_visits * 0.08))] <- NA

# Test dates: one date per visit, with various formats and invalid entries
visit_dates <- rep(as.Date(NA), n_visits)
for (p in seq_len(n_patients)) {
  p_idx <- which(patient_ids == sprintf("P%03d", p))
  visit_dates[p_idx] <- Sys.Date() - sample(1:365, length(p_idx), replace = FALSE)
}
date_strings <- ifelse(runif(n_visits) < 0.5,
  format(visit_dates, "%Y-%m-%d"), # ISO format
  format(visit_dates, "%Y/%m/%d") # Slash format
)
invalid_idx <- sample(n_visits, round(n_visits * 0.1))
date_strings[invalid_idx] <- sample(
  c("N/A", "unknown", "pending", "", NA_character_),
  length(invalid_idx), replace = TRUE
)

# Create long-format lab data: all tests are recorded at each visit
lab_data <- data.frame(
  patient_id = rep(patient_ids, each = 3),
  test = rep(c("Glucose", "Creatinine", "Cholesterol"), times = n_visits),
  value = as.character(c(rbind(glucose_vals, creatinine_vals, cholesterol_vals))),
  unit = c(rbind(glucose_units, creatinine_units, cholesterol_units)),
  test_date = rep(date_strings, each = 3)
)

# Add messy string values that need cleaning
# Glucose with text annotations and European decimal commas
messy_glucose_idx <- sample(which(lab_data$test == "Glucose"), 15)
lab_data$value[messy_glucose_idx] <- sample(c(
  "<40", ">500", "5.2", "6.8", "120 mg/dL", "89 mmol/L",
  "5.2 (fasting)", "180 (post-meal)", "N/A", "pending",
  "6..5", "7..2", "8.5.1", "normal range", "see comment"
), length(messy_glucose_idx), replace = TRUE)

# Creatinine with text annotations
messy_creatinine_idx <- sample(which(lab_data$test == "Creatinine"), 12)
lab_data$value[messy_creatinine_idx] <- sample(c(
  "<0.5", ">5.0", "1.1", "0.9", "1.2 mg/dL", "110 umol/L",
  "1.5 (dialysis)", "2.8 (critical)", "N/A", " hemolyzed",
  "1.3 ", " 1.4", "(1.2)", "1.0*", "see note"
), length(messy_creatinine_idx), replace = TRUE)

# Cholesterol with text annotations
messy_cholesterol_idx <- sample(which(lab_data$test == "Cholesterol"), 10)
lab_data$value[messy_cholesterol_idx] <- sample(c(
  "<100", ">400", "4.5", "5.2", "200 mg/dL", "5.5 mmol/L",
  "180 (fasting)", "250 (borderline)", "N/A", "lipemic"
), length(messy_cholesterol_idx), replace = TRUE)

knitr::kable(head(lab_data[sample(nrow(lab_data)), ], 10),
  caption = "Original Messy Lab Data (10 random rows)"
)

## ----data-overview------------------------------------------------------------
overview <- data_overview(lab_data)

print(overview$variable_types)
print(overview$summary_stats)
print(overview$quality_issues$missing_values)

## ----check-units--------------------------------------------------------------
knitr::kable(unit_view(lab_data, subject_col = "test", value_col = "value", unit_col = "unit"),
  caption = "Unit Conflicts by Test Type"
)

## ----check-nonnum-------------------------------------------------------------
nonnum_df <- df_view_nonnum(lab_data)
knitr::kable(head(nonnum_df, 15), caption = "Non-numeric Entries by Variable")

## ----extract-numeric----------------------------------------------------------
# Create a copy for cleaning
lab_data_cleaned <- lab_data

# Extract numeric values from the messy value column
lab_data_cleaned$value_numeric <- extract_num(lab_data$value)

# Show before/after comparison
comparison <- data.frame(
  test = lab_data$test,
  original = lab_data$value,
  cleaned = lab_data_cleaned$value_numeric,
  unit = lab_data$unit
) %>%
  dplyr::filter(original != cleaned | is.na(cleaned)) %>%
  head(15)

knitr::kable(comparison, caption = "Value Cleaning: Before vs After")

## ----date-conversion----------------------------------------------------------
# Convert various date formats to standard Date objects
lab_data_cleaned$test_date_clean <- to_date(lab_data$test_date)

# Show date conversion results
date_comparison <- data.frame(
  original = lab_data$test_date,
  cleaned = lab_data_cleaned$test_date_clean
) %>%
  dplyr::filter(!is.na(original)) %>%
  head(15)

knitr::kable(date_comparison, caption = "Date Conversion: Before vs After")

## ----check-invalid-dates------------------------------------------------------
invalid_dates <- lab_data_cleaned %>%
  dplyr::filter(!is.na(test_date) & is.na(test_date_clean)) %>%
  dplyr::select(patient_id, test, test_date) %>%
  head(10)

knitr::kable(invalid_dates, caption = "Invalid Date Entries (Could Not Be Parsed)")

## ----update-cleaned-data------------------------------------------------------
lab_data <- lab_data_cleaned %>%
  dplyr::mutate(
    value = value_numeric,
    test_date = test_date_clean
  ) %>%
  dplyr::select(-value_numeric, -test_date_clean)

## ----unit-standardize-apply---------------------------------------------------
change_rules <- list(
  list(subject = "Glucose", target_unit = "mg/dL", units2change = "mmol/L", coeffs = 18),
  list(subject = "Creatinine", target_unit = "mg/dL", units2change = "umol/L", coeffs = 1 / 88.4),
  list(subject = "Cholesterol", target_unit = "mg/dL", units2change = "mmol/L", coeffs = 38.67)
)

lab_data_std <- unit_standardize(
  lab_data,
  subject_col = "test", value_col = "value",
  unit_col = "unit", change_rules = change_rules
)

knitr::kable(head(lab_data_std, 15), caption = "Lab Data After Unit Standardization")

## ----verify-standardization---------------------------------------------------
knitr::kable(unit_view(lab_data_std, subject_col = "test", value_col = "value", unit_col = "unit"),
  caption = "Units After Standardization"
)

## ----outlier-detection--------------------------------------------------------
# Split data by test type for outlier detection
lab_data_clean <- lab_data_std

# Detect outliers for each test
for (test_name in c("Glucose", "Creatinine", "Cholesterol")) {
  test_data <- lab_data_std$value[lab_data_std$test == test_name]

  outlier_res <- detect_outliers(test_data, method = "iqr")

  cat("\n", test_name, ":\n")
  cat("  Total values:", length(test_data), "\n")
  cat("  Outliers detected:", sum(outlier_res$outlier_mask, na.rm = TRUE), "\n")
  cat("  Missing values:", sum(is.na(test_data)), "\n")
}

## ----compare-methods----------------------------------------------------------
glucose_values <- lab_data_std$value[lab_data_std$test == "Glucose"]

knitr::kable(data.frame(
  Method = c("MAD", "IQR", "Z-score"),
  Outlier_Count = c(
    sum(mad_outlier(glucose_values, threshold = 3), na.rm = TRUE),
    sum(iqr_outlier(glucose_values, threshold = 1.5), na.rm = TRUE),
    sum(zscore_outlier(glucose_values, threshold = 3), na.rm = TRUE)
  )
), caption = "Outlier Detection Results by Method (Glucose)")

## ----handle-outliers----------------------------------------------------------
# Create a copy for outlier handling
lab_data_final <- lab_data_std

# Set outliers to NA for each test type
for (test_name in c("Glucose", "Creatinine", "Cholesterol")) {
  test_idx <- lab_data_std$test == test_name
  test_values <- lab_data_std$value[test_idx]

  outlier_res <- detect_outliers(test_values, method = "iqr")

  # Set outliers to NA
  lab_data_final$value[test_idx][outlier_res$outlier_mask] <- NA
}

# Compare before and after outlier handling
comparison_df <- data.frame(
  Test = rep(c("Glucose", "Creatinine", "Cholesterol"), each = 2),
  Stage = rep(c("Before", "After"), 3),
  N = c(
    sum(!is.na(lab_data_std$value[lab_data_std$test == "Glucose"])),
    sum(!is.na(lab_data_final$value[lab_data_final$test == "Glucose"])),
    sum(!is.na(lab_data_std$value[lab_data_std$test == "Creatinine"])),
    sum(!is.na(lab_data_final$value[lab_data_final$test == "Creatinine"])),
    sum(!is.na(lab_data_std$value[lab_data_std$test == "Cholesterol"])),
    sum(!is.na(lab_data_final$value[lab_data_final$test == "Cholesterol"]))
  ),
  Mean = c(
    mean(lab_data_std$value[lab_data_std$test == "Glucose"], na.rm = TRUE),
    mean(lab_data_final$value[lab_data_final$test == "Glucose"], na.rm = TRUE),
    mean(lab_data_std$value[lab_data_std$test == "Creatinine"], na.rm = TRUE),
    mean(lab_data_final$value[lab_data_final$test == "Creatinine"], na.rm = TRUE),
    mean(lab_data_std$value[lab_data_std$test == "Cholesterol"], na.rm = TRUE),
    mean(lab_data_final$value[lab_data_final$test == "Cholesterol"], na.rm = TRUE)
  )
)

knitr::kable(comparison_df, caption = "Before vs After Outlier Handling", digits = 2)

## ----wide-format--------------------------------------------------------------
# Convert to wide format preserving all test dates
# This creates a longitudinal cohort dataset
lab_wide <- lab_data_final %>%
  filter(!is.na(test_date)) %>% # Remove records without valid dates
  select(patient_id, test_date, test, value) %>%
  tidyr::pivot_wider(
    names_from = test,
    values_from = value,
    names_sort = TRUE
  )

# Sort by patient and date for longitudinal view
lab_wide <- lab_wide %>%
  arrange(patient_id, test_date)

test_cols <- c("Glucose", "Creatinine", "Cholesterol")

knitr::kable(head(lab_wide, 10), caption = "Longitudinal Lab Data (First 10 records)")

## ----cohort-structure---------------------------------------------------------
cohort_summary <- lab_wide %>%
  group_by(patient_id) %>%
  summarise(
    n_visits = n(),
    first_date = min(test_date),
    last_date = max(test_date)
  ) %>%
  head(10)

knitr::kable(cohort_summary, caption = "Cohort Structure: Visits per Patient")

## ----missing-patterns---------------------------------------------------------
missing_summary <- data.frame(
  Variable = test_cols,
  Missing_Count = sapply(lab_wide[test_cols], function(x) sum(is.na(x))),
  Missing_Percent = round(sapply(lab_wide[test_cols], function(x) {
    sum(is.na(x)) / length(x) * 100
  }), 2)
)

knitr::kable(missing_summary, caption = "Missing Data Summary by Test")

## ----filter-missing-----------------------------------------------------------
valid_idx <- get_valid_subset(
  lab_wide[test_cols],
  row_na_ratio = 0.5, # Allow up to 50% missing per record
  col_na_ratio = 0.3, # Allow up to 30% missing per test type
  return_index = TRUE
)

lab_wide_clean <- lab_wide[
  valid_idx$rows,
  c("patient_id", "test_date", test_cols[valid_idx$cols])
]

cat(
  "Dimensions:", nrow(lab_wide), "records",
  "->", nrow(lab_wide_clean), "records\n"
)
cat(
  "Unique patients:", length(unique(lab_wide$patient_id)),
  "->", length(unique(lab_wide_clean$patient_id)), "\n"
)

## ----clinical-categories------------------------------------------------------
# Glucose categories
lab_wide_clean$glucose_category <- cut_by(
  lab_wide_clean$Glucose,
  breaks = c(100, 126),
  labels = c("Normal", "Prediabetes", "Diabetes"),
  label_with_range = FALSE
)

# Creatinine categories (eGFR approximation)
lab_wide_clean$renal_function <- cut_by(
  lab_wide_clean$Creatinine,
  breaks = c(1.2, 2.0, 4.0),
  labels = c("Normal", "Mild", "Moderate", "Severe"),
  label_with_range = FALSE
)

# Cholesterol categories
lab_wide_clean$cholesterol_category <- cut_by(
  lab_wide_clean$Cholesterol,
  breaks = c(200, 240),
  labels = c("Desirable", "Borderline", "High"),
  label_with_range = FALSE
)

# Summary of categories across all visits
cat_summary <- data.frame(
  Category = c(
    rep("Glucose", length(table(lab_wide_clean$glucose_category))),
    rep("Renal Function", length(table(lab_wide_clean$renal_function))),
    rep("Cholesterol", length(table(lab_wide_clean$cholesterol_category)))
  ),
  Level = c(
    names(table(lab_wide_clean$glucose_category)),
    names(table(lab_wide_clean$renal_function)),
    names(table(lab_wide_clean$cholesterol_category))
  ),
  Count = c(
    as.vector(table(lab_wide_clean$glucose_category)),
    as.vector(table(lab_wide_clean$renal_function)),
    as.vector(table(lab_wide_clean$cholesterol_category))
  )
)

knitr::kable(cat_summary, caption = "Clinical Category Distributions (All Visits)")

## ----longitudinal-view--------------------------------------------------------
# Select the patient with the most visits for demonstration
patient_with_most_visits <- lab_wide_clean %>%
  count(patient_id, sort = TRUE) %>%
  slice(1) %>%
  pull(patient_id)

patient_trajectory <- lab_wide_clean %>%
  filter(patient_id == patient_with_most_visits) %>%
  select(
    patient_id, test_date, Glucose, Creatinine, Cholesterol,
    glucose_category, renal_function
  ) %>%
  arrange(test_date)

knitr::kable(patient_trajectory, caption = "Example: Single Patient's Lab Trajectory")

## ----complete-pipeline--------------------------------------------------------
# Step 1: Data Overview
overview <- data_overview(lab_data)

# Step 2: Format Cleaning - Extract numeric values and convert dates
clean <- lab_data
clean$value <- extract_num(lab_data$value)
# `lab_data` has already been cleaned above, so coerce to character before re-parsing
clean$test_date <- to_date(as.character(lab_data$test_date))

# Step 3: Unit Standardization
change_rules <- list(
  list(subject = "Glucose", target_unit = "mg/dL", units2change = "mmol/L", coeffs = 18),
  list(subject = "Creatinine", target_unit = "mg/dL", units2change = "umol/L", coeffs = 1 / 88.4),
  list(subject = "Cholesterol", target_unit = "mg/dL", units2change = "mmol/L", coeffs = 38.67)
)

clean <- unit_standardize(
  clean,
  subject_col = "test", value_col = "value",
  unit_col = "unit", change_rules = change_rules
)

# Step 4: Outlier Detection and Handling
for (test_name in c("Glucose", "Creatinine", "Cholesterol")) {
  test_idx <- clean$test == test_name
  test_values <- clean$value[test_idx]
  outlier_res <- detect_outliers(test_values, method = "iqr")
  clean$value[test_idx][outlier_res$outlier_mask] <- NA
}

# Step 5: Convert to wide format (longitudinal cohort data)
clean_wide <- clean %>%
  filter(!is.na(test_date)) %>%
  select(patient_id, test_date, test, value) %>%
  tidyr::pivot_wider(names_from = test, values_from = value) %>%
  arrange(patient_id, test_date)

# Step 6: Missing value filtering (only the test-result columns)
valid_idx <- get_valid_subset(
  clean_wide[test_cols],
  row_na_ratio = 0.5, col_na_ratio = 0.3,
  return_index = TRUE
)
clean_wide <- clean_wide[
  valid_idx$rows,
  c("patient_id", "test_date", test_cols[valid_idx$cols])
]

# Step 7: Create clinical categories
clean_wide$glucose_category <- cut_by(
  clean_wide$Glucose,
  breaks = c(100, 126),
  labels = c("Normal", "Prediabetes", "Diabetes")
)

# Step 8: Final check
final_overview <- data_overview(clean_wide)
knitr::kable(final_overview$summary_stats, caption = "Final Data Quality Overview")

cat(
  "\nOriginal records:", nrow(lab_data),
  "| Final records:", nrow(clean_wide),
  "| Removed:", nrow(lab_data) - nrow(clean_wide), "\n"
)
cat(
  "Unique patients:", length(unique(lab_data$patient_id)),
  "->", length(unique(clean_wide$patient_id)), "\n"
)


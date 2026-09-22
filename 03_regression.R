# Title: SEP, socioenvironment, and SCD among midlife and older LGBTQIA+ adults in California 
# Purpose: Regression
# Date: 2025-07-22

# Set up environment -----------------------------------------------------
library(tidyverse)
library(here)
library(mice)
library(miceadds)
library(marginaleffects)

# load data
file_date <- "2025-07-22"

df <- readr::read_csv(
  here::here("data", paste0("df_reg_cda_", file_date, ".csv")),
  show_col_types = F
) |> 
  janitor::clean_names() 

# MICE -------------------------------------------------------------------
# Multiple imputation to account for 18% of any missing data
pred <- mice::make.predictorMatrix(df)
meth <- mice::make.method(df)

df_imp <- mice(
  df,
  m = 20, 
  seed = 123, 
  predictorMatrix = pred,
  method = meth,
  printFlag = F
)
rm(pred, meth)

# Regression -------------------------------------------------------------
exposures <- c(
  "ed_levels", "no_work", "income_cat", "assets_cat",
  "finance_insecure_cat", "food_insecure_cat", "unstable_bin",
  "public_transport", "safe_cat"
)
covariates <- c(
  "age_new", "gender_grp", "race_poc", "mil_service", "trauma_bin",
  "caregiver", "support_bin", "urban", "hiv_pos", "hear_vis_loss"
)

# Helper to fit glm across imputed datasets
fit_model <- function(exposure, adjust = FALSE) {
  rhs <- if (adjust) paste(c(exposure, covariates), collapse = " + ") else exposure
  fmla <- as.formula(paste("scd ~", rhs))
  with(df_imp, glm(fmla, family = binomial()))
}

# Unadjusted models
m0 <- purrr::map(exposures, fit_model, adjust = F)
names(m0) <- exposures

# Adjusted models
m1 <- purrr:::map(exposures, fit_model, adjust = T)
names(m1) <- exposures

# Helper to run avg_comparisons for one model
get_model_est <- function(model, exposure, model_label) {
  marginaleffects::avg_comparisons(
    model,
    variables  = exposure,
    comparison = "lnoravg",
    transform  = "exp"
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(model = model_label)
}

# Run across all models in each list
df_mod_out <- dplyr::bind_rows(
  purrr::imap_dfr(m0, ~ get_model_est(.x, .y, "0")),
  purrr::imap_dfr(m1, ~ get_model_est(.x, .y, "1"))
)

# Run regression model that additionally adjust for mental health and substance use
covariates <- c(covariates, "lon", "phq", "pcl", "audit", "current_smoke")

# Sensitivity analysis: adjusted model + mental health/substance use
m2 <- purrr:::map(exposures, fit_model, adjust = T)
names(m2) <- exposures
df_sen <- purrr::imap_dfr(m2, ~ get_model_est(.x, .y, "sen_anl"))

# Save outputs -----------------------------------------------------------
file_date <- "2025-07-22"

write.csv(
  df_mod_out, 
  here::here("output", paste0("table4_main_reg_", file_date, ".csv")),
  row.names = F
)

write.csv(
  df_sen, 
  here::here("output", paste0("table5_sens_reg_", file_date, ".csv")),
  row.names = F
)
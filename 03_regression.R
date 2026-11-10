# Title: SEP, socioenvironment, and SCD among midlife and older LGBTQIA+ adults in California 
# Purpose: Regression
# Date: 2026-07-22

# Set up environment -----------------------------------------------------
library(tidyverse)
library(here)
library(mice)
library(miceadds)
library(marginaleffects)

# load data
file_date <- "2026-10-27"

df <- readr::read_csv(
  here::here("data", paste0("df_reg_cda_", file_date, ".csv")),
  show_col_types = F
) |> 
  janitor::clean_names() |> 
  dplyr::mutate(
    dplyr::across(c(ed_levels:current_smoke), ~as.factor(.)),
    ed_levels = forcats::fct_relevel(ed_levels, "4"),
    income_cat = forcats::fct_relevel(income_cat, "5"),
    assets_cat = forcats::fct_relevel(assets_cat, "4")
  ) 

# MICE -------------------------------------------------------------------
# Multiple imputation to account for 18% of any missing data
pred <- mice::make.predictorMatrix(df)
meth <- mice::make.method(df)

# Run mice in parallel
df_imp <- mice::futuremice(
  df, 
  m = 20, 
  parallelseed = 123, 
  n.core = 8, 
  predictorMatrix = pred,
  method = meth
)

rm(pred, meth)

# Regression -------------------------------------------------------------
exposures <- c(
  "ed_levels", "no_work", "income_cat", "assets_cat",
  "finance_insecure_cat", "food_insecure_cat", "unstable_bin",
  "public_transport", "safe_cat"
)
covariates <- c(
  "age_cat", "gender_grp", "race_poc", "usborn", "mil_service", "urban", "hiv_pos"
)

# Helper to fit glm across imputed datasets
fit_model <- function(exposure, adjust = F) {
  rhs <- if (adjust) paste(c(exposure, covariates), collapse = " + ") else exposure
  fmla_str <- paste("scd ~", rhs)
  with(df_imp, glm(as.formula(fmla_str), family = binomial()))
}

# Unadjusted models
m0 <- purrr::map(exposures, fit_model, adjust = F)
names(m0) <- exposures

# Adjusted models
m1 <- purrr::map(exposures, fit_model, adjust = T)
names(m1) <- exposures

# Helper to run avg_comparisons for one model
get_model_est <- function(model, exposure, model_label) {
  do.call(
    marginaleffects::avg_comparisons,
    list(
      model      = model,
      variables  = exposure,
      comparison = "lnoravg"
    )
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(model = model_label)
}

# Run across all models in each list
df_mod_out <- dplyr::bind_rows(
  purrr::imap_dfr(m0, ~ get_model_est(.x, .y, "0")),
  purrr::imap_dfr(m1, ~ get_model_est(.x, .y, "1"))
) |>
  dplyr::mutate(
    OR      = exp(estimate),
    OR_low  = exp(conf.low),
    OR_high = exp(conf.high),
    OR_ci   = sprintf("%.2f (%.2f, %.2f)", OR, OR_low, OR_high)
  )

# Run regression model that additionally adjust for mental health and substance use
covariates <- c(covariates, "lon", "phq", "pcl", "audit", "current_smoke")

# Sensitivity analysis: adjusted model + mental health/substance use
m2 <- purrr::map(exposures, fit_model, adjust = T)
names(m2) <- exposures
df_sen <- purrr::imap_dfr(m2, ~ get_model_est(.x, .y, "sen_anl")) |> 
  dplyr::mutate(
    OR      = exp(estimate),
    OR_low  = exp(conf.low),
    OR_high = exp(conf.high),
    OR_ci   = sprintf("%.2f (%.2f, %.2f)", OR, OR_low, OR_high)
  )

# Save outputs -----------------------------------------------------------
file_date <- "2026-10-27"

saveRDS(
  df_imp,
  here::here("data", paste0("df_imp_mids_", file_date, ".rds"))
)

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
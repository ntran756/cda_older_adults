# Title: SEP, socioenvironment, and SCD among midlife and older LGBTQIA+ adults in California 
# Purpose: Dominance analysis
# Date: 2026-08-03

# Set up environment -----------------------------------------------------
library(tidyverse)
library(here)
library(dominanceanalysis)
library(modelsummary)

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

df_imp <- readRDS(
  here::here("data", paste0("df_imp_mids_", file_date, ".rds"))
)

# Fit full model from main analysis --------------------------------------
predictors <- c(
  "ed_levels", "no_work", "income_cat", "assets_cat", "finance_insecure_cat",
  "food_insecure_cat", "unstable_bin", "public_transport", "safe_cat",
  "age_cat", "gender_grp", "race_poc", "usborn", "urban", "mil_service", "hiv_pos"
)
formula <- as.formula(paste("scd ~", paste(predictors, collapse = " + ")))

# Fit logistic model with all SEP/environmental factors and relevant confounders
# Complete case
m_cc <- glm(formula, data = df, family = binomial())

# MI
m_imp <- mice::pool(with(df_imp, glm(scd ~ ed_levels + no_work + income_cat + assets_cat + finance_insecure_cat +
                         food_insecure_cat + unstable_bin + public_transport + safe_cat +
                         age_cat + race_poc + gender_grp + usborn + urban + mil_service + hiv_pos, 
                         family = binomial())))

# Explicit lookup: term name -> meaningful label
rename_var <- function(old_names) {
  label_map <- c(
    "ed_levels1"             = "HS diploma/GED or less",
    "ed_levels2"             = "Some college",
    "ed_levels3"             = "4-year degree",
    "no_work1"               = "Unemployment",
    "income_cat1"            = "$0-30,000",
    "income_cat2"            = "$30,0001-60,000",
    "income_cat3"            = "$60,001-90,000",
    "income_cat4"            = "$90,001-120,000",
    "assets_cat1"            = "$0-9,999",
    "assets_cat2"            = "$10,000-100,000",
    "assets_cat3"            = "$100,001-500,000",
    "finance_insecure_cat1"  = "Financial insecurity",
    "food_insecure_cat1"     = "Food insecurity",
    "unstable_bin1"          = "Unstably housed",
    "public_transport1"      = "Relies on public transportation",
    "safe_cat2"              = "Safe for SM only",
    "safe_cat3"              = "Safe for GM only",
    "safe_cat4"              = "Safe for both",
    "age_new2"               = "\u226565 years",
    "race_poc1"              = "Minoritized ethnoracial group",
    "gender_grp2"            = "Cisgender woman",
    "gender_grp3"            = "Gender diverse AFAB",
    "gender_grp4"            = "Gender diverse AMAB",
    "gender_grp5"            = "Transgender man",
    "gender_grp6"            = "Transgender woman",
    "usborn1"                = "US born",
    "urban1"                 = "Urban residence",
    "mil_service1"           = "Military service",
    "hiv_pos1"               = "Living with HIV"
  )

# Map old names to labels; keep original if not found
new_names <- ifelse(old_names %in% names(label_map),
                      label_map[old_names],
                      old_names)
  setNames(new_names, old_names)
}

modelsummary::modelsummary(
  list("Complete case" = m_cc, "Imputed" = m_imp),
  shape = term ~ statistic,
  fmt = modelsummary::fmt_decimal(2,3), 
  statistic = c("conf.int", "p.value"),
  coef_omit = "Intercept",
  exponentiate = T, 
  coef_rename = rename_var,
  output = here::here("output", "full_model_ouput_2026-10-27.csv")
)

# Dominance analysis -----------------------------------------------------
# Bootstrapped dominance analysis: decomposes model fit to rank predictors 
# by relative importance (pseudo r2)
# Covariates forced into every model, NOT ranked
# Dominance package runs boot internally, use parallel operation within boot
boot_da <- dominanceanalysis::bootAverageDominanceAnalysis(
  m_cc,                    
  R = 1000,            
  constants = c("age_cat", "gender_grp", "race_poc", "usborn", "mil_service", "urban", "hiv_pos"), 
  parallel = "multicore",
  ncpus = 8
)
# Get general dominance values based on mcfadden's r2
df_da_boot <- boot_da[["boot"]][["t"]] |> 
  as.data.frame()

# Sensitivity analysis: adjusted models + mental health/substance use
predictors <- c(predictors, "lon", "phq", "pcl", "audit", "current_smoke")
formula <- as.formula(paste("scd ~", paste(predictors, collapse = " + ")))

m_sen <- glm(formula, data = df, family = binomial())
sen_boot_da <- dominanceanalysis::bootAverageDominanceAnalysis(
  m_sen,                    
  R = 1000,            
  constants = c(       
    "age_cat", "gender_grp", "race_poc", "usborn", "mil_service", "urban", "hiv_pos", 
    "lon", "phq", "pcl", "audit", "current_smoke"
  ),
  parallel = "multicore",
  ncpus = 8
)
# Get general dominance values based on mcfadden's r2
df_sen_da_boot <- sen_boot_da[["boot"]][["t"]] |> 
  as.data.frame()

# Save output ------------------------------------------------------------
file_date <- "2026-10-27"

write.csv(
  df_da_boot, 
  here::here("output", paste0("da_boot_results_", file_date, ".csv")),
  row.names = F
)

write.csv(
  df_sen_da_boot, 
  here::here("output", paste0("sen_da_boot_results_", file_date, ".csv")),
  row.names = F
)

# Title: SEP, socioenvironment, and SCD among midlife and older LGBTQIA+ adults in California 
# Purpose: Dominance analysis
# Date: 2025-08-03

# Set up environment -----------------------------------------------------
library(tidyverse)
library(here)
library(dominanceanalysis)
library(future)
library(modelsummary)

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

# Fit full model from main analysis --------------------------------------
predictors <- c(
  "ed_levels", "no_work", "income_cat", "assets_cat", "finance_insecure_cat",
  "food_insecure_cat", "unstable_bin", "public_transport", "safe_cat",
  "age_new", "gender_grp", "race_poc", "mil_service", "trauma_bin",
  "support_bin", "urban", "caregiver", "hiv_pos", "hear_vis_loss"
)
formula <- as.formula(paste("scd ~", paste(predictors, collapse = " + ")))

# Fit logistic model with all SEP/environmental factors and relevant confounders
# Complete case
m_cc <- glm(formula, data = df, family = binomial())

# MI
m_imp <- pool(with(df_imp, glm(scd ~ ed_levels + no_work + income_cat + assets_cat + finance_insecure_cat +
                         food_insecure_cat + unstable_bin + public_transport + safe_cat +
                         age_new + race_poc + gender_grp + 
                         urban + mil_service +
                         support_bin + caregiver + 
                         trauma_bin +  hiv_pos + hear_vis_loss, 
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
    "urban1"                 = "Urban residence",
    "mil_service1"           = "Military service",
    "support_bin1"           = "Has social support",
    "caregiver1"             = "Caregiver",
    "trauma_bin1"            = "History of trauma",
    "hiv_pos1"               = "HIV positive",
    "hear_vis_loss1"         = "Hearing or vision loss"
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
  fmt = fmt_decimal(2,3), 
  statistic = c("conf.int", "p.value"),
  coef_omit = "Intercept",
  exponentiate = T, 
  coef_rename = rename_var,
  output = here::here("output", "full_model_ouput_2025-08-13.csv")
)

# Dominance analysis -----------------------------------------------------
# Bootstrapped dominance analysis: decomposes model fit to rank predictors 
# by relative importance (pseudo r2)
# Covariates forced into every model, NOT ranked
boot_da <- dominanceanalysis::bootAverageDominanceAnalysis(
  m,                    
  R = 1000,            
  constants = c(       
    "age_new", "gender_grp", "race_poc", "mil_service", "trauma_bin",
    "support_bin", "urban", "caregiver", "hiv_pos", "hear_vis_loss"
  )
)
# Get general dominance values based on mcfadden's r2
df_da_boot <- boot_da[["boot"]][["t"]] |> 
  as.data.frame()

# Save output ------------------------------------------------------------
file_date <- "2025-08-03"

write.csv(
  df_da_boot, 
  here::here("output", paste0("da_boot_results_", file_date, ".csv")),
  row.names = F
)

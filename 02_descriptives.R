# Title: SEP, socioenvironment, and SCD among midlife and older LGBTQIA+ adults in California 
# Purpose: Descriptive analysis
# Date: 2025-07-22

# Set up environment -----------------------------------------------------
library(tidyverse)
library(here)
library(tableone)

# load data
file_date <- "2025-07-03"

df <- readr::read_csv(
  here::here("data", paste0("df_cda_older_adults_", file_date, ".csv")),
  show_col_types = F
) |> 
  janitor::clean_names() 

# Prep data for analysis -------------------------------------------------
dplyr::count(df, memloss)

# Exclude people with missing data for subjective cognitive decline (`memloss`)
df <- df |> dplyr::filter(!is.na(memloss)) # n=168 excluded

# Recode all socioeconomic/environment from numeric to factor
df <- df |> 
  dplyr::mutate(
    dplyr::across(c(ed_levels,income_cat,assets_cat,safe_cat), ~as.factor(.)),
    ed_levels = forcats::fct_relevel(ed_levels, "4"),
    income_cat = forcats::fct_relevel(income_cat, "5"),
    assets_cat = forcats::fct_relevel(assets_cat, "4")
  ) 

# Define variable list for analysis
df <- df |> 
  dplyr::select(
    ed_levels, no_work, income_cat, assets_cat, finance_insecure_cat, food_insecure_cat,
    unstable_bin, public_transport, safe_cat, scd, 
    age_new, gender_grp, race_poc, mil_service, trauma_bin, urban, 
    support_bin, caregiver, 
    hiv_pos, hear_vis_loss, current_smoke, lon, phq, pcl, audit
  ) |>
    dplyr::mutate(dplyr::across(c(ed_levels:current_smoke), ~as.factor(.x)))

# Descriptive analysis ---------------------------------------------------
# Check total missing
1-sum(complete.cases(df))/nrow(df)

# Participant characteristics by SCD
v_names <- df |>
  dplyr::select(
    age_new, race_ethn_1:race_ethn_8, race_multi, race_poc, race_miss,
    gender_grp, intersex, asexual:another, so_multi, so_miss, usborn,
    mil_service, insured, urban, trauma_bin, support_bin, caregiver,
    current_smoke, hiv_pos, hear_vis_loss, lon, phq, pcl, audit
  ) |> names()

v_fct_names <- v_names[!v_names %in% c("lon", "phq", "pcl", "audit")]

tab1 <- tableone::CreateTableOne(
  vars = v_names,
  factorVars = v_fct_names,
  strata = "scd",
  data = df,
  test = T,
  addOverall = T,
  includeNA = T
)

# check missingness
summary(tab1$ContTable)

# SEP and socioenvironment factors by SCD
v_names <- df |>
  dplyr::select(
    ed_levels, no_work, income_cat, assets_cat, finance_insecure_cat, 
    food_insecure_cat, unstable_bin, unstable, public_transport, safe_cat
  ) |> names()

tab2 <- tableone::CreateTableOne(
  vars = v_names,
  factorVars = v_names,
  strata = "scd",
  data = df,
  test = T,
  addOverall = T,
  includeNA = T
)

# Examine correlation matrix between SEP and socioenvironmental factors
# Reverse order of safe_cat
df <- df |>
  dplyr::mutate(unsafe_cat = as.numeric(forcats::fct_rev(factor(safe_cat))))

vars <- c(
  "ed_levels",
  "no_work",
  "income_cat",
  "assets_cat",
  "finance_insecure_cat",
  "food_insecure_cat",
  "unstable_bin",
  "public_transport",
  "unsafe_cat"
)

# Get all unique pairs of variables
pairs <- combn(vars, 2, simplify = FALSE)

# Run loop to calculate spearman correlation for each pair
df_cor <- lapply(pairs, function(pair) {
  v1 <- df[[pair[1]]]
  v2 <- df[[pair[2]]]
  
  # Find pairwise complete cases
  complete_cases <- complete.cases(v1, v2)
  pairwise_n <- sum(complete_cases)
  
  # Calculate correlation on complete cases
  cor_test <- cor.test(v1[complete_cases], v2[complete_cases], method = "spearman", exact = F)
  
  list(
    var1 = pair[1],
    var2 = pair[2],
    spearman_rho = cor_test$estimate,
    spearman_p_value = cor_test$p.value,
    pairwise_n = pairwise_n
  )
}) |> dplyr::bind_rows()

# Save outputs -----------------------------------------------------------
file_date <- "2025-07-22"

write.csv(
  tab1, 
  here::here("output", paste0("table1_part_character_", file_date, ".csv"))
)

write.csv(
  tab2, 
  here::here("output", paste0("table2_sep_enviro_", file_date, ".csv"))
)

write.csv(
  df_cor, 
  here::here("output", paste0("table3_cor_matrix_", file_date, ".csv")),
  row.names = F
)

# Save data --------------------------------------------------------------
write.csv(
  df, 
  here::here("data", paste0("df_reg_cda_", file_date, ".csv")),
  row.names = F
)
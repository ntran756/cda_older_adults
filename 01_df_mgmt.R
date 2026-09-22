# Title: SEP, socioenvironment, and SCD among midlife and older LGBTQIA+ adults in California 
# Purpose: Data cleaning 
# Date: 2025-07-03


# Set up environment -----------------------------------------------------
library(tidyverse)
library(here)

# Load data
df_raw <- readr::read_csv(
  here::here("data", "CDA Survey Deidentified Dataset.csv"), 
  show_col_types = F
) |> 
  janitor::clean_names() 

# Create analytic data file with relevant variables
# Exclude people with missing data for `memloss`
df <- df_raw |> 
  dplyr::select(
    age_new, race_ethn_1:race_ethn_8, orientation_1:orientation_11, asexual:another, 
    genderid_1:genderid_12, gender_cat, saab, intersex, birthplace, density, milstatus, 
    trauma, insurance, support, caregiver_rec, caregiver, hivstatus, dis_omb1:dis_omb6,
    lonely1:lonely3, phq_1, phq_2, pcl_1:pcl_6, audit1:audit3, smoker, smoke_now, 
    ed_levels, work, income, assets, finance_insecure, food_insecure,
    unstable, transport_how5r, access, safe_gm_cat, safe_sm_cat, memloss
  ) 

rm(df_raw)


# Data cleaning ----------------------------------------------------------
# Recode variables as needed for analysis
# Cognitive decline, SEP, environmental 
df <- df |> 
  dplyr::mutate(
    scd = ifelse(memloss == 1, 1, 0),
    scd2 = ifelse(memloss %in% c(1,3), 1, 0),
    no_work = ifelse(work == 0, 1, 0),
    income_cat = dplyr::case_when(
      is.na(income) ~ NA, 
      income %in% 1:4 ~ 1, 
      income %in% 5:7 ~ 2,
      income %in% 8:10 ~ 3,
      income %in% 11:13 ~ 4,
      T ~ 5
    ),
    assets_cat = dplyr::case_when(
      assets %in% 2:3 ~ 2, 
      assets == 4 ~ 3, 
      assets == 5 ~ 4, 
      T ~ assets
    ),
    finance_insecure_cat = ifelse(finance_insecure == 3, 0, 1),
    food_insecure_cat = ifelse(food_insecure == 5, 0, 1),
    unstable_bin = ifelse(unstable == 1, 0, 1),
    public_transport = transport_how5r,
    no_hosp = ifelse(access == 2, 1, 0),
    safe_cat = dplyr::case_when(
      safe_sm_cat == 1 & safe_gm_cat == 1 ~ 4,                            # safe both
      (safe_sm_cat %in% 2:3 | is.na(safe_sm_cat)) & safe_gm_cat == 1 ~ 3, # safe gm
      safe_sm_cat == 1 & (safe_gm_cat %in% 2:3 | is.na(safe_gm_cat)) ~ 2, # safe sm
      safe_sm_cat %in% 2:3 | safe_gm_cat %in% 2:3 ~ 1                     # not safe
    )
  )

# Mental health, substance use, and physical conditions that are potential risk factors
# for SCD and dementia 
df <- df |> 
  dplyr::mutate(
    current_smoke = dplyr::case_when(
      smoke_now %in% 2:3 ~ 3, 
      smoke_now == 1 & smoker == 1 ~ 2,
      smoke_now == 1 & smoker == 2 ~ 1, 
      is.na(smoke_now) & smoker == 1 ~ 2,
      is.na(smoke_now) & smoker == 2 ~ 1,
      smoke_now == 1 & is.na(smoker) ~ 1
    ),
    ever_smoke = dplyr::case_when(current_smoke %in% 2:3 ~ 1, current_smoke == 1 ~ 0),
    phq = rowSums(dplyr::across(phq_1:phq_2)),
    lon = rowSums(dplyr::across(lonely1:lonely3)),
    pcl = rowSums(dplyr::across(pcl_1:pcl_6)),
    audit2 = ifelse(audit1 == 0 & is.na(audit2), 0, audit2),
    audit3 = ifelse(audit1 == 0 & is.na(audit3), 0, audit3),
    audit = rowSums(dplyr::across(audit1:audit3)),
    dep = ifelse(phq >= 3, 1, 0),
    lonely = ifelse(lon >= 6, 1, 0),
    ptsd = ifelse(pcl >= 14, 1, 0),
    audit_geq3 = ifelse(audit >= 3, 1, 0),
    audit_geq4 = ifelse(audit >= 4, 1, 0),
    hiv_pos = ifelse(hivstatus == 2, 1, 0),
    deaf = ifelse(dis_omb1 == 1, 1, 0),
    blind = ifelse(dis_omb2 == 1, 1, 0),
    hear_vis_loss = dplyr::case_when(
      dis_omb1 == 1 | dis_omb2 == 1 ~ 1, 
      is.na(dis_omb1) & is.na(dis_omb2) ~ NA, 
      T ~ 0
    )
  )

# Participant characteristics 
df <- df |> 
  dplyr::mutate(
    # recode so that NAs are 0
    dplyr::across(race_ethn_1:race_ethn_8, ~ ifelse(is.na(.), 0, .)),
    dplyr::across(genderid_1:genderid_12, ~ ifelse(is.na(.), 0, .)),
    dplyr::across(orientation_1:orientation_11, ~ ifelse(is.na(.), 0, .)),
    # multiracial groups/missing demographics
    race_multi = ifelse(rowSums(dplyr::across(race_ethn_1:race_ethn_8), na.rm = T) > 1, 1, 0),
    race_miss = ifelse(rowSums(dplyr::across(race_ethn_1:race_ethn_8), na.rm = T) == 0, 1, 0),
    race_poc = ifelse(race_ethn_7 == 1 & race_multi == 0, 0, 1),
    gi_multi = ifelse(rowSums(dplyr::across(genderid_1:genderid_12), na.rm = T) > 1, 1, 0),
    gi_miss = ifelse(rowSums(dplyr::across(genderid_1:genderid_12), na.rm = T) == 0, 1, 0),
    so_multi = ifelse(rowSums(dplyr::across(orientation_1:orientation_11), na.rm = T) > 1, 1, 0),
    so_miss = ifelse(rowSums(dplyr::across(orientation_1:orientation_11), na.rm = T) == 0, 1, 0),
    intersex = ifelse(intersex == 1, 1, 0),
    # other characteristics
    usborn = ifelse(birthplace == 1, 1, 0),
    urban = ifelse(density == "Urban", 1, 0),
    mil_service = ifelse(milstatus == 1, 1, 0),
    trauma_bin = dplyr::case_when(trauma %in% 4:5 ~ 1, trauma == 6 ~ 0),
    insured = ifelse(insurance == 1, 1, 0),
    support_cat = dplyr::case_when(
      support %in% 1:2 ~ 1, 
      support == 3 ~ 2,
      support %in% 4:5 ~ 3,
    ),
    support_bin = dplyr::case_when(support %in% 4:5 ~ 1, support %in% 1:3 ~ 0),
    get_caregive = ifelse(caregiver_rec == 1, 1, 0),
    caregiver = ifelse(caregiver == 1, 1, 0),
    # reorder gender groups: 1 cisman, 2 ciswomn, 3 gdafab, 4 gdamab, 5 transman, 6 transwoman
    gender_grp = dplyr::case_when(
      gender_cat == 3 ~ 5, 
      gender_cat == 4 ~ 6, 
      gender_cat == 5 ~ 3, 
      gender_cat == 6 ~ 4, 
      T ~ gender_cat
    ), 
    trans_gd = ifelse(gender_cat %in% 1:2, 0, 1)
  )

# Save data --------------------------------------------------------------
file_date <- "2025-07-03"

write.csv(
  df, 
  here::here("data", paste0("df_cda_older_adults_", file_date, ".csv")),
  row.names = F
)
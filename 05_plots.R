# Title: SEP, socioenvironment, and SCD among midlife and older LGBTQIA+ adults in California 
# Purpose: Create plots
# Date: 2026-09-08

# Set up environment -----------------------------------------------------
library(tidyverse)
library(here)
library(ggforestplot)
library(ggforce)

# Helper to read a csv from output
load_output <- function(file) {
  readr::read_csv(here("output", file), show_col_types = F) |>
    janitor::clean_names()
}

# load data
df_cor <- load_output("table3_cor_matrix_2026-07-22.csv")
df_reg <- load_output("table4_main_reg_2026-07-22.csv")
df_da  <- load_output("da_boot_results_2026-08-03.csv") 

# Plot correlation matrix ------------------------------------------------
var_labels <- c(
  "ed_levels" = "Education",
  "no_work" = "Unemployment",
  "income_cat" = "Income",
  "assets_cat" = "Assets",
  "finance_insecure_cat" = "Financial insecurity",
  "food_insecure_cat" = "Food insecurity",
  "unstable_bin" = "Unstably housed",
  "public_transport" = "Relies on public transportation",
  "unsafe_cat" = "Lack LGBTQIA+ community safety"
)

var_pos <- setNames(seq_along(vars), vars)

df_heat <- df_cor |>
  dplyr::select(var1, var2, spearman_rho, spearman_p_value, pairwise_n) |>
  # add diagonal (self-correlation = 1)
  dplyr::bind_rows(
    tibble::tibble(var1 = vars, var2 = vars,
                   spearman_rho = 1, spearman_p_value = NA, pairwise_n = NA)
  ) |>
  # ensure each pair is oriented so var1 is "later" than var2 (lower triangle)
  dplyr::mutate(
    p1 = var_pos[var1],
    p2 = var_pos[var2]
  ) |>
  # swap so the higher-position var is always var1 (keeps lower triangle)
  dplyr::mutate(
    v1 = ifelse(p1 <= p2, var1, var2),
    v2 = ifelse(p1 <= p2, var2, var1)
  ) |>
  dplyr::distinct(v1, v2, .keep_all = TRUE) |>
  dplyr::mutate(
    var1 = factor(v1, levels = vars),
    var2 = factor(v2, levels = rev(vars)),
    stars = dplyr::case_when(
      is.na(spearman_p_value) ~ "",
      spearman_p_value < 0.001 ~ "***",
      spearman_p_value < 0.01  ~ "**",
      spearman_p_value < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

p_cor <- ggplot(df_heat, aes(x = var1, y = var2, fill = spearman_rho)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(
    aes(label = ifelse(
      is.na(pairwise_n),
      sprintf("%.2f", spearman_rho),                            
      sprintf("%.2f%s\nn=%.0f", spearman_rho, stars, pairwise_n)  
    )),
    size = 2.8, lineheight = 0.9
  ) +
  scale_fill_gradient2(
    low = "#D95F02", mid = "white", high = "#2166AC",
    midpoint = 0, limits = c(-1, 1), expression("Spearman" ~ rho)
  ) +
  scale_x_discrete(labels = var_labels, expand = c(0, 0)) +           
  scale_y_discrete(labels = var_labels, expand = c(0, 0)) + 
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 0, size = 9),
    plot.margin = margin(5, 5, 5, 5)
  ) 
p_cor

# Forest plot of main regression model -----------------------------------
df_reg <- df_reg |> 
  dplyr::mutate(
    model = factor(
      rep(c("m1", "m0"), each = 18), 
      labels = c("Unadjusted", "Adjusted")
    ), 
    contrast2 = dplyr::case_when(
      term == "ed_levels" & contrast == "ln(odds(1) / odds(4))" ~ "HS Diploma/GED or less",
      term == "ed_levels" & contrast == "ln(odds(2) / odds(4))" ~ "Some college",
      term == "ed_levels" & contrast == "ln(odds(3) / odds(4))" ~ "4-year degree",
      term == "no_work" ~ "Yes",
      term == "income_cat" & contrast == "ln(odds(1) / odds(5))" ~ "$0-30,000",
      term == "income_cat" & contrast == "ln(odds(2) / odds(5))" ~ "$30,001-60,000",
      term == "income_cat" & contrast == "ln(odds(3) / odds(5))" ~ "$60,001-90,000",
      term == "income_cat" & contrast == "ln(odds(4) / odds(5))" ~ "$90,001-120,000",
      term == "assets_cat" & contrast == "ln(odds(1) / odds(4))" ~ "$0-9,999",
      term == "assets_cat" & contrast == "ln(odds(2) / odds(4))" ~ "$10,000-100,000",
      term == "assets_cat" & contrast == "ln(odds(3) / odds(4))" ~ "$100,001-500,000",
      term == "finance_insecure_cat" ~ "Yes",
      term == "food_insecure_cat" ~ "Yes",
      term == "unstable_bin" ~ "Yes",
      term == "public_transport" ~ "Yes",
      term == "safe_cat" & contrast == "ln(odds(2) / odds(1))" ~ "SM only",
      term == "safe_cat" & contrast == "ln(odds(3) / odds(1))" ~ "GM only",
      term == "safe_cat" & contrast == "ln(odds(4) / odds(1))" ~ "Safe for both"
    ),
    term2 = dplyr::case_when(
      term == "ed_levels" ~ "Education levels (Ref = Graduate degree)",
      term == "no_work" ~ "Unemployment (Ref = No)",
      term == "income_cat" ~ "Household income (Ref = \u2265$120,001)",
      term == "assets_cat" ~ "Household assets (Ref = \u2265$500,001)",
      term == "finance_insecure_cat" ~ "Financial insecurity (Ref = No)",
      term == "food_insecure_cat" ~ "Food insecurity (Ref = No)",
      term == "unstable_bin" ~ "Unstably housed (Ref = No)",
      term == "public_transport" ~ "Relies on public transportation (Ref = No)",
      term == "safe_cat" ~ "LGBTQIA+ community safety (Ref = Not safe)"
    ),
    term2 = factor(term2, levels = c(
      "Education levels (Ref = Graduate degree)",
      "Unemployment (Ref = No)",
      "Household income (Ref = \u2265$120,001)",
      "Household assets (Ref = \u2265$500,001)",
      "Financial insecurity (Ref = No)",
      "Food insecurity (Ref = No)",
      "Unstably housed (Ref = No)",
      "Relies on public transportation (Ref = No)",
      "LGBTQIA+ community safety (Ref = Not safe)"
    ))
  )

p_reg <- ggforestplot::forestplot(
  df = df_mod_out,
  name = contrast2,
  estimate = estimate,
  se = std.error,
  pvalue = p.value,
  psignif = 1,
  colour = model,
  shape = model,
  xlab = "Odds ratio for subjective cognitive decline (95% CI)",
  logodds = F
) +   
  scale_color_manual(
    values = c("#2C7FB8", "#D95F02"),
    labels = c("Adjusted", "Unadjusted")
  ) +
  scale_shape_manual(
    values = c(15,17),
    labels = c("Adjusted", "Unadjusted")
  ) +
  ggforce::facet_col(~ term2,  scales = "free_y", space = "free") +
  theme(
    strip.placement = "outside",
    strip.background = ggplot2::element_blank(),
    axis.text.y = ggtext::element_markdown(), 
    strip.text.y.left = element_text(angle = 0),
    legend.title = element_blank(),
    text = element_text(size = 11), 
    legend.text = element_text(size = 11), 
    legend.position = "bottom",
    plot.margin = margin(t = 2, r = 5, b = 2, l = 2),   
    panel.spacing = unit(2, "pt"),                      
    strip.text = element_text(margin = margin(t = 1, b = 1)),  
    legend.margin = margin(t = 0, b = 0),                
    legend.box.spacing = unit(2, "pt"),                  
    axis.title.x = element_text(margin = margin(t = 4))
  ) +
  guides(
    shape = guide_legend(override.aes = list(size = 2), reverse = T),
    colour = guide_legend(override.aes = list(size = 2), reverse = T)
  )
p_reg

# Forest plot of dominance analysis --------------------------------------
df_df_boot_long <- df_da_boot |> 
  tibble::as_tibble() |> 
  dplyr::select(v1:v9) |> 
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "exposure",
    values_to = "value"
  ) |> 
  dplyr::mutate(
    exposure = dplyr::case_when(
      exposure == "V1" ~ "Education levels",
      exposure == "V2" ~ "Unemployment",
      exposure == "V3" ~ "Household income",
      exposure == "V4" ~ "Household assets",
      exposure == "V5" ~ "Financial insecurity",
      exposure == "V6" ~ "Food insecurity",
      exposure == "V7" ~ "Unstably housed",
      exposure == "V8" ~ "Relies on public transportation",
      exposure == "V9" ~ "LGBTQIA+ community safety"
    )
  )

df_summary <- df_df_boot_long |>
  dplyr::group_by(exposure) |>
  dplyr::summarise(
    estimate = median(value, na.rm = T),  
    lower = quantile(value, 0.025, na.rm = T),
    upper = quantile(value, 0.975, na.rm = T)
  )

p_dom <- ggplot(df_summary, aes(x = forcats::fct_reorder(exposure, estimate), y = estimate * 100)) +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100),
                width = 0.2, linewidth = 0.6, color = "black") +
  geom_point(size = 2, color = "black", shape = 15) +
  geom_text(
  aes(label = sprintf("%.1f (%.1f, %.1f)", estimate * 100, lower * 100, upper * 100)),  
  nudge_y = 0.26, vjust = -1, size = 3, color = "black", fontface = "bold"
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.18))        
  ) +
  labs(
    x = NULL,
    y = expression("McFadden's " * R^2 * " (%) with bootstrap 95% percentile CI")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_text(size = 10, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
    axis.line.x = element_line(color = "#4D4F53", linewidth = 0.2),   
    axis.ticks.x = element_line(color = "#4D4F53", linewidth = 0.2),  
    panel.grid.major.x = element_line(linetype = "solid", color = "grey90", linewidth = 0.2)
  )
p_dom

# Save outputs -----------------------------------------------------------
file_date <- "2026-09-08"

ggplot2::ggsave(
  here::here("output", paste0("spearmen_cor_", file_date, ".png")),
  plot = p_cor,
  height = 150,
  width = 200,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  here::here("output", paste0("regression_forestplot_", file_date, ".png")),
  plot = p_reg, 
  height = 205, 
  width = 140, 
  units = "mm", 
  dpi = 600, 
  bg = "white"
)

ggplot2::ggsave(
  here::here("output", paste0("dominance_analy_", file_date, ".png")),
  plot = p_dom, 
  height = 100, 
  width = 165, 
  units = "mm", 
  dpi = 600, 
  bg = "white"
)

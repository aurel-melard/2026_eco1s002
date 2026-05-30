# -------------------------------------------------------------------------
#
#   Education and Inequality - 1. Data Cleaning
#
#   Loads the raw World Bank panel, creates derived variables,
#   and saves a clean analysis dataset.
#
#   Bachelor of Sciences - Ecole Polytechnique
#   ECO 1S002 2026
#
#   Jane Doe & John Smith
#   May 2026
#
# -------------------------------------------------------------------------

rm(list = ls())
gc()

## 1. Paths and packages ----

input  <- '/path/to/mock_project/raw_data'
output <- '/path/to/mock_project/output'

pacman::p_load(tidyverse, readr)

## 2. Load raw data ----

dt_raw <- read_csv(paste0(input, "/world_bank_panel.csv"))

head(dt_raw)
summary(dt_raw)

## 3. Clean and create variables ----

dt_clean <- dt_raw %>%
  rename(
    edu_exp  = edu_exp_pct_gdp,
    gdp_pc   = gdp_pc_usd,
    sec_enr  = secondary_enroll,
    urban    = urban_pop_pct
  ) %>%
  mutate(
    log_gdp_pc = log(gdp_pc),
    country    = factor(country),
    year       = as.integer(year),
    year_fct   = factor(year)
  ) %>%
  arrange(country, year)

## 4. Check for missing values ----

dt_clean %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  filter(n_missing > 0)

## 5. Save clean dataset ----

saveRDS(dt_clean, file = paste0(output, "/clean_panel.rds"))
message("Clean data saved to: ", output, "/clean_panel.rds")

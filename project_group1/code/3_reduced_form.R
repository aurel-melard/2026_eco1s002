# -------------------------------------------------------------------------
#
#   Education and Inequality - 3. Reduced Form Estimation
#
#   Runs OLS and country fixed-effects regressions of the Gini index
#   on government education expenditure and control variables.
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

output <- '/path/to/mock_project/output'

pacman::p_load(tidyverse, stargazer, fixest)

## 2. Load clean data ----

dt <- readRDS(paste0(output, "/clean_panel.rds"))

## 3. OLS regressions (Table 2) ----

reg_ols1 <- lm(gini ~ edu_exp,                               data = dt)
reg_ols2 <- lm(gini ~ edu_exp + log_gdp_pc,                  data = dt)
reg_ols3 <- lm(gini ~ edu_exp + log_gdp_pc + sec_enr + urban, data = dt)

stargazer(
  reg_ols1, reg_ols2, reg_ols3,
  type  = "latex",
  out   = paste0(output, "/table2_ols.tex"),

  title           = "OLS estimates of education expenditure on income inequality",
  label           = "tab:ols",

  dep.var.labels  = "Gini index",
  covariate.labels = c(
    "Education exp. (\\% GDP)",
    "Log GDP per capita",
    "Secondary enrollment",
    "Urban population (\\%)"
  ),
  column.labels   = c("Baseline", "+ Log GDP", "Full controls"),

  digits          = 3,
  header          = FALSE,
  no.space        = TRUE,
  keep.stat       = c("n", "rsq"),
  star.cutoffs    = c(0.05, 0.01, 0.001),
  notes           = c("*** p<0.001, ** p<0.01, * p<0.05"),
  notes.append    = FALSE
)

## 4. Country fixed-effects regressions (Table 3) ----

reg_fe1 <- feols(gini ~ edu_exp                                | country,              data = dt)
reg_fe2 <- feols(gini ~ edu_exp + log_gdp_pc                   | country,              data = dt)
reg_fe3 <- feols(gini ~ edu_exp + log_gdp_pc + sec_enr + urban | country + year_fct,   data = dt)

etable(
  reg_fe1, reg_fe2, reg_fe3,
  tex        = TRUE,
  title      = "Fixed-effects estimates of education expenditure on income inequality",
  label      = "tab:fe",
  coefstat   = "se",
  fitstat    = ~ n + r2,
  depvar     = FALSE,
  headers    = list("Country FE" = c("Yes", "Yes", "Yes"),
                    "Year FE"    = c("No",  "No",  "Yes")),
  file       = paste0(output, "/table3_fe.tex"),
  replace    = TRUE
)

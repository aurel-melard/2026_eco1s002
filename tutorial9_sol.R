# -------------------------------------------------------------------------
#
#   Tutorial 9 - Solution
#
#   This code gives the solution to Tutorial 9 Exercise
#
#   Bachelor of Sciences - Ecole Polytechnique
#   ECO 1S002 2026
#
#   Aurel Mélard (aurel.melard@polytechnique.fr)
#   April 14, 2026
#
# -------------------------------------------------------------------------

rm(list = ls())
gc()

## 1. Path and packages ----

input  <- "/Users/aurelmelard/Dropbox/cours/Topics/2026_eco1s002/data"
output <- "/Users/aurelmelard/Dropbox/cours/Topics/tuto_solutions/output"

pacman::p_load(tidyverse,
               fixest)


## 2. Load data ----
df <- read_csv(paste0(input, "/lfs_women.csv"), show_col_types = FALSE)

## 3. Create variables ----
df <- df %>%
  mutate(
    lfp         = as.integer(ntravail == 1),  # 1 if working
    woman       = as.integer(nsexe == 2),     # female
    child       = as.integer(mne1 >= 1),      # has at least one child
    large       = as.integer(mne1 > 2),       # large family (>2 children)
    woman_child = woman * child,              # motherhood (woman x child)
    woman_large = woman * large,              # mother in large family
    twin        = as.integer(hh_twin_after > 0),
    woman_twin  = woman * twin,               # instrument for woman_large
    woman_ssex  = woman * same_sexe          # instrument for woman_large
  )

# IV sample: restrict to households with >= 2 children.
# In this sub-sample child = 1 for all, so woman_child = woman (drop it).
# Endogenous variables: large, woman_large (two instruments each).
df_iv <- df %>% filter(mne1 >= 2)

## 4. OLS baseline ----
ols1 <- feols(lfp ~ woman + child + woman_child + large + woman_large,
              data = df)
summary(ols1)

## 5. OLS with controls ----
ols2 <- feols(lfp ~ woman + child + woman_child + large + woman_large +
                nag + ndiplo,
              data = df)
summary(ols2)

## 6. OLS with household fixed effects ----
# child and large absorbed by FE (constant within HH).
# woman, woman_child, woman_large vary within HH (wife vs husband).
ols3 <- feols(lfp ~ woman + woman_child + woman_large + nag + ndiplo | idmen,
              data = df)
summary(ols3)

## 7. IV: twin instrument ----
# Two endogenous variables (large, woman_large) -> two instruments (twin, woman_twin).
iv1 <- feols(lfp ~ woman + nag + ndiplo |
               large + woman_large ~ twin + woman_twin,
             data = df_iv)
summary(iv1)

## 8. IV: same-gender instrument ----
iv2 <- feols(lfp ~ woman + nag + ndiplo |
               large + woman_large ~ same_sexe + woman_ssex,
             data = df_iv)
summary(iv2)


## Results table ----
etable(ols1, ols2, ols3, iv1, iv2,
       headers = c("OLS", "OLS + controls", "OLS + HH FE",
                   "IV (twins)", "IV (same gender)"),
       keep = c("woman", "child", "woman_child", "large", "woman_large"),
       digits = 3)

## First-stage F-statistics ----
cat("\nFirst-stage F (twins):\n")
print(fitstat(iv1, "ivf"))
cat("\nFirst-stage F (same gender):\n")
print(fitstat(iv2, "ivf"))

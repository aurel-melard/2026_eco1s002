# Tutorial 11: Assortative Matching by Education and City Type
# Data: data_couples.dta (French Labour Force Survey)
# Years: 1996, 2002, 2006, 2013
rm(list = ls())
gc()

library(haven)
library(dplyr)
library(ggplot2)
library(broom)
library(car)

# Load and prepare ----

d <- read_dta("data/data_couples.dta") |>
  mutate(
    city_type = factor(
      city_size,
      levels = c(0, 1, 4, 6, 8),
      labels = c("Rural", "<5k", "5-20k", "20-200k", ">200k")
    )
  )

cat("Dimensions:", nrow(d), "x", ncol(d), "\n")
cat("Years:", unique(d$year), "\n")
cat("City types:", levels(d$city_type), "\n")


# Q0: Research question ----
# Do larger cities produce more positively assortative marriage markets?
# This is descriptive, not causal: education, city type, and partner
# education are jointly determined by self-selection.
# Thick market hypothesis: larger cities -> larger pool of potential
# partners -> easier to find a close match -> steeper matching gradient.


# Q1: Simple matching gradient ----
# Slope on education_r: a one-year increase in own education is
# associated with (slope) more years of education in the partner.
# beta = 1 -> perfect PAM; beta = 0 -> random matching.
# Limitation: conflates matching with city-sorting and cohort effects.

m0 <- lm(education_p ~ education_r, data = d, weights = weight)
summary(m0)

# Q2: Baseline specification ----
# Expected signs:
#   beta1 (education_r): > 0  positive assortative matching
#   beta2 (city_size):   > 0  more educated partners in urban areas
#   beta3 (age_r):       < 0  older cohorts have lower education
#   beta4 (active_r):    > 0  active workers have more educated partners

m1 <- lm(
  education_p ~ education_r + city_size + age_r + active_r,
  data = d,
  weights = weight
)
summary(m1)


# Q3: Interaction with city type ----
# delta_c = coefficient on (education_r x city_type_c):
# how much steeper the gradient is in city type c vs Rural.
# Positive and increasing delta_c supports the thick market hypothesis.

m2 <- lm(
  education_p ~ education_r * city_type + age_r + active_r,
  data = d,
  weights = weight
)
summary(m2)

interaction_terms <- grep(
  "education_r:city_type", names(coef(m2)), value = TRUE
)
linearHypothesis(m2, interaction_terms)


# Q4: Add year fixed effects ----
# Year FE absorb trends common to all couples: rising educational
# attainment over time, business cycle effects, survey design changes.

m3 <- lm(
  education_p ~ education_r * city_type + age_r + active_r + factor(year),
  data = d,
  weights = weight
)
summary(m3)

cat("Without year FE: beta1 =", round(coef(m2)["education_r"], 3), "\n")
cat("With    year FE: beta1 =", round(coef(m3)["education_r"], 3), "\n")


# Q5: Plot matching gradient by city type ----
# Separate weighted OLS per city type (with controls and year FE),
# extract slope on education_r, plot with 95% CIs.

grad_by_city <- d |>
  group_by(city_type) |>
  group_modify(~ {
    fit <- lm(
      education_p ~ education_r + age_r + active_r + factor(year),
      data = .x,
      weights = .x$weight
    )
    tidy(fit, conf.int = TRUE) |>
      filter(term == "education_r") |>
      select(estimate, conf.low, conf.high, std.error, p.value)
  })

print(grad_by_city)

ggplot(
  grad_by_city,
  aes(x = city_type, y = estimate, ymin = conf.low, ymax = conf.high)
) +
  geom_point(size = 3, colour = "steelblue") +
  geom_errorbar(width = 0.15, colour = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(
    x        = "City type",
    y        = "Matching gradient (slope on own education)",
    title    = "Assortative matching gradient by city type",
    subtitle = "Controls: age, active status, year FE; weighted regressions"
  ) +
  theme_bw(base_size = 12)


# Q6: Bonus - partner's labour force status ----
# LPM with active_p as outcome.
# Slope on education_r: more educated respondents have more active
# partners - a second dimension of assortative matching.

m_bonus <- lm(
  active_p ~ education_r * city_type + age_r + active_r + factor(year),
  data = d,
  weights = weight
)
summary(m_bonus)

grad_active <- d |>
  group_by(city_type) |>
  group_modify(~ {
    fit <- lm(
      active_p ~ education_r + age_r + active_r + factor(year),
      data = .x,
      weights = .x$weight
    )
    tidy(fit, conf.int = TRUE) |>
      filter(term == "education_r") |>
      select(estimate, conf.low, conf.high)
  })

ggplot(
  grad_active,
  aes(x = city_type, y = estimate, ymin = conf.low, ymax = conf.high)
) +
  geom_point(size = 3, colour = "tomato") +
  geom_errorbar(width = 0.15, colour = "tomato") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  labs(
    x        = "City type",
    y        = "Slope on own education",
    title    = "Education-activity matching gradient by city type",
    subtitle = "LPM for partner's labour force status; controls as above"
  ) +
  theme_bw(base_size = 12)


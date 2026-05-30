# -------------------------------------------------------------------------
#
#   Education and Inequality - 2. Descriptive Evidence
#
#   Produces summary statistics and descriptive figures.
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

pacman::p_load(tidyverse, xtable, wesanderson)

## 2. Load clean data ----

dt <- readRDS(paste0(output, "/clean_panel.rds"))

## 3. Summary statistics table ----

summary_stats <- dt %>%
  select(gini, edu_exp, log_gdp_pc, sec_enr, urban) %>%
  summarise(across(
    everything(),
    list(
      Min    = ~ min(., na.rm = TRUE),
      Q1     = ~ quantile(., 0.25, na.rm = TRUE),
      Mean   = ~ mean(., na.rm = TRUE),
      Median = ~ median(., na.rm = TRUE),
      Q3     = ~ quantile(., 0.75, na.rm = TRUE),
      Max    = ~ max(., na.rm = TRUE),
      N      = ~ sum(!is.na(.))
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(), names_to = c("variable", "stat"), names_sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value)

summary_stats$variable <- c(
  "Gini index",
  "Education expenditure (% GDP)",
  "Log GDP per capita",
  "Secondary enrollment (% gross)",
  "Urban population (%)"
)

print(
  xtable(summary_stats,
         caption = "Summary statistics, 10 countries, 2015--2019",
         label   = "tab:summary_stats",
         digits  = 2),
  type              = "latex",
  include.rownames  = FALSE,
  file              = paste0(output, "/table1_summary_stats.tex")
)

## 4. Figure 1: Education spending vs. Gini (scatter) ----

ggplot(dt, aes(x = edu_exp, y = gini, color = country, label = iso3c)) +
  geom_point(size = 2.5, shape = 1) +
  geom_text(nudge_y = 0.6, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "grey50", linetype = "dashed", linewidth = 0.6) +
  theme_minimal() +
  labs(
    x     = "Government expenditure on education (% of GDP)",
    y     = "Gini index",
    title = "Education spending and income inequality",
    subtitle = "10 countries, 2015–2019"
  ) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    legend.position = "none"
  )

ggsave(paste0(output, "/figure1_scatter_edu_gini.pdf"), width = 16, height = 10, unit = "cm")

## 5. Figure 2: Gini trends over time ----

country_colors <- setNames(
  wes_palette("Moonrise1", n = 10, type = "continuous"),
  levels(dt$country)
)

ggplot(dt, aes(x = year, y = gini, color = country, group = country)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_color_manual(values = country_colors) +
  theme_minimal() +
  labs(
    x      = "Year",
    y      = "Gini index",
    color  = "Country",
    title  = "Income inequality over time",
    subtitle = "10 countries, 2015–2019"
  ) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, face = "italic"),
    legend.position = "right"
  )

ggsave(paste0(output, "/figure2_gini_trends.pdf"), width = 20, height = 10, unit = "cm")

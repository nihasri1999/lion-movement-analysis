# ==============================================================================
# Script: 04_issf_analysis.R
# Purpose: Generate control steps, calculate time of day, and fit iSSF.
# ==============================================================================

# 1. Load Required Libraries
library(tidyverse)
library(amt)
library(lubridate)

# 2. Read Step Dataset from Script 02/03
lion_steps <- readRDS("data/processed/lion_steps_30m.rds")

# 3. Generate 10 Available Control Steps per Observed Step
set.seed(42)
lion_issf_data <- lion_steps %>%
  filter(!is.na(ta_)) %>%
  nest(data = -id) %>%
  mutate(
    rnd_steps = map(data, ~ random_steps(.x, n_control = 10))
  ) %>%
  select(id, rnd_steps) %>%
  unnest(cols = rnd_steps)

# 4. Add Time-of-Day (Day vs. Night)
# Categorizes step start time into Day (06:00 to 18:00) vs Night
lion_issf_data <- lion_issf_data %>%
  mutate(
    hour_ = hour(t1_),
    tod_  = factor(ifelse(hour_ >= 6 & hour_ < 18, "day", "night"))
  )

# 5. Fit Movement iSSF
# Evaluates if step length log(sl_) and direction cos(ta_) change by day/night
m_issf <- lion_issf_data %>%
  fit_issf(
    case_ ~ log(sl_) * tod_ + cos(ta_) * tod_ + strata(step_id_)
  )

# 6. Save Prepped Data and Fitted Model
saveRDS(lion_issf_data, file = "data/processed/lion_issf_prepared.rds")
saveRDS(m_issf, file = "data/processed/lion_issf_model.rds")

# 7. Print Model Summary
summary(m_issf)
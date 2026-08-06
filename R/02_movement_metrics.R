# ==============================================================================
# Script: 02_movement_metrics.R
# Purpose: Convert regularized point tracks into step-based trajectories
#          and extract step lengths (sl_) and turn angles (ta_).
# ==============================================================================

# 1. Load Required Libraries
library(tidyverse)
library(amt)

# 2. Read Clean Resampled Dataset from Phase 1
lion_resampled <- readRDS("data/processed/lion_resampled_30m.rds")

# 3. Convert Points to Steps by Burst
# steps_by_burst() calculates sl_ (meters) and ta_ (radians) per continuous burst
lion_steps <- lion_resampled %>%
  nest(data = -id) %>%
  mutate(
    steps = map(data, ~ steps_by_burst(.x))
  ) %>%
  select(id, steps) %>%
  unnest(cols = steps)

# 4. Save Steps Object for Integrated Step-Selection Analysis (Phase 3)
saveRDS(lion_steps, file = "data/processed/lion_steps_30m.rds")

# 5. Quick Diagnostic Summaries
# Summary stats of step length (meters) across individuals
summary(lion_steps$sl_)

# Count how many total steps were created
nrow(lion_steps)
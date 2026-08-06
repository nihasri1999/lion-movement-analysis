# ==============================================================================
# Script: 05_publication_figures.R
# Purpose: Generate publication-ready figures for HMM behavioral states 
#          and iSSF movement selection models (Polished Labels).
# ==============================================================================

# 1. Load Required Libraries
library(tidyverse)
library(momentuHMM)
library(amt)
library(grid)

# Ensure output directory exists
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# 2. Load Processed Objects
hmm_decoded <- readRDS("data/processed/lion_hmm_decoded.rds")
m_hmm       <- readRDS("data/processed/lion_hmm_model.rds")
m_issf      <- readRDS("data/processed/lion_issf_model.rds")

# Label HMM numeric states with names
state_names <- c("1" = "Resting", "2" = "Localized", "3" = "Transit")
hmm_decoded <- hmm_decoded %>%
  mutate(state_label = factor(state_names[as.character(decoded_state)], 
                              levels = c("Resting", "Localized", "Transit")))

# ------------------------------------------------------------------------------
# FIGURE 1: Combined 2-Panel HMM Distributions
# ------------------------------------------------------------------------------
# Panel A: Step Length Distribution
p1a_step <- ggplot(hmm_decoded, aes(x = step, fill = state_label)) +
  geom_density(alpha = 0.6) +
  scale_x_log10() +
  scale_fill_manual(values = c("Resting" = "#E69F00", "Localized" = "#009E73", "Transit" = "#D55E00")) +
  labs(title = "A) Step Length Distribution by HMM State", x = "Step Length (m, log scale)", y = "Density") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

# Panel B: Turning Angle Distribution (Clean Pi Radians Axis)
p1b_angle <- hmm_decoded %>%
  filter(!is.na(angle)) %>%
  ggplot(aes(x = angle, fill = state_label)) +
  geom_density(alpha = 0.6) +
  scale_x_continuous(
    breaks = c(-pi, -pi/2, 0, pi/2, pi),
    labels = c("-π", "-π/2", "0", "π/2", "π")
  ) +
  scale_fill_manual(values = c("Resting" = "#E69F00", "Localized" = "#009E73", "Transit" = "#D55E00")) +
  labs(title = "B) Turning Angle Distribution by HMM State", x = "Turning Angle (radians)", y = "Density", fill = "Behavioral State") +
  theme_minimal(base_size = 11)

# Stack vertically using built-in grid grobs
g1 <- ggplotGrob(p1a_step)
g2 <- ggplotGrob(p1b_angle)
fig1_stacked <- rbind(g1, g2, size = "max")

ggsave("outputs/figures/Fig1_hmm_state_distributions.png", fig1_stacked, width = 8, height = 8, dpi = 300)

# ------------------------------------------------------------------------------
# FIGURE 2: Spatial Trajectory Decoded by Behavioral State (Kilometers)
# ------------------------------------------------------------------------------
sample_id <- names(which.max(table(hmm_decoded$ID)))

p2_track <- hmm_decoded %>%
  filter(ID == sample_id) %>%
  slice(1:1000) %>%
  mutate(
    x_km = x / 1000,
    y_km = y / 1000
  ) %>%
  ggplot(aes(x = x_km, y = y_km)) +
  geom_path(color = "grey70", linewidth = 0.4) +
  geom_point(aes(color = state_label), size = 1.5, alpha = 0.8) +
  scale_color_manual(values = c("Resting" = "#E69F00", "Localized" = "#009E73", "Transit" = "#D55E00")) +
  labs(
    title = paste0("Decoded Lion Movement Path (Individual ", sample_id, ")"),
    x = "UTM Easting (km)", y = "UTM Northing (km)", color = "Behavioral State"
  ) +
  theme_classic(base_size = 11) +
  coord_equal()

ggsave("outputs/figures/Fig2_hmm_decoded_track.png", p2_track, width = 8, height = 6, dpi = 300)

# ------------------------------------------------------------------------------
# FIGURE 3: iSSF Relative Selection Strength (Descriptive Labels)
# ------------------------------------------------------------------------------
coef_df <- as.data.frame(summary(m_issf)$coefficients) %>%
  rownames_to_column(var = "Parameter") %>%
  mutate(
    HR = `exp(coef)`,
    lower = exp(coef - 1.96 * `se(coef)`),
    upper = exp(coef + 1.96 * `se(coef)`)
  )

p3_issf <- ggplot(coef_df, aes(x = reorder(Parameter, HR), y = HR)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_pointrange(aes(ymin = lower, ymax = upper), color = "#2B5C8F", linewidth = 0.8) +
  scale_x_discrete(labels = c(
    "tod_night"          = "Night Baseline",
    "cos(ta_)"           = "Day Directional Persistence",
    "log(sl_)"           = "Day Step Length",
    "log(sl_):tod_night" = "Night × Step Length",
    "tod_night:cos(ta_)" = "Night × Directional Persistence"
  )) +
  coord_flip() +
  labs(
    title = "iSSF Movement Parameter Estimates (95% CI)",
    x = "Behavioral Parameter",
    y = "Relative Selection Strength (Hazard Ratio)"
  ) +
  theme_classic(base_size = 11)

ggsave("outputs/figures/Fig3_issf_coefficients.png", p3_issf, width = 7.5, height = 4.5, dpi = 300)

message("Polished figures successfully updated in outputs/figures/")
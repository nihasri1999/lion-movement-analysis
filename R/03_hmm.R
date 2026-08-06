# ==============================================================================
# Script: 03_hmm.R
# Purpose: Fit a 3-State Hidden Markov Model (HMM) to classify behavioral states
#          (Resting, Localized Movement, Directed Transit) using momentuHMM.
# ==============================================================================

# 1. Load Required Libraries
library(tidyverse)
library(momentuHMM)

# 2. Read Resampled Point Dataset from Phase 1
lion_resampled <- readRDS("data/processed/lion_resampled_30m.rds")

# 3. Format Data Frame for momentuHMM
# momentuHMM requires explicit x, y coordinates and an ID column
df_hmm <- lion_resampled %>%
  select(ID = id, x = x_, y = y_, date_time = t_) %>%
  as.data.frame()

# Prepare trajectory metrics (step length and turn angle) inside momentuHMM
hmm_prep <- prepData(df_hmm, type = "UTM", coordNames = c("x", "y"))

# 4. Set Initial Parameters for 3 Behavioral States
# Step Length Parameters (Gamma Distribution: 9 total parameters)
mu0        <- c(10, 250, 1200)   # Initial means (meters per 30 mins)
sigma0     <- c(15, 200, 800)    # Initial standard deviations
zeromass0  <- c(0.30, 0.05, 0.01) # Zero-mass probabilities
stepPar0   <- c(mu0, sigma0, zeromass0)

# Turning Angle Parameters (von Mises Distribution: 3 concentration parameters)
kappa0     <- c(0.1, 0.5, 1.8)   # State 1 (dispersed/random) -> State 3 (highly directional)
anglePar0  <- kappa0

# 5. Fit 3-State Hidden Markov Model
m_hmm <- fitHMM(
  data = hmm_prep,
  nbStates = 3,
  dist = list(step = "gamma", angle = "vm"),
  Par0 = list(step = stepPar0, angle = anglePar0),
  stateNames = c("Resting", "Localized", "Transit")
)

# 6. Decode Most Likely State Sequence (Viterbi Algorithm)
hmm_prep$decoded_state <- viterbi(m_hmm)

# Calculate state probabilities per step
state_probs <- stateProbs(m_hmm)
colnames(state_probs) <- c("prob_resting", "prob_localized", "prob_transit")
hmm_decoded <- cbind(hmm_prep, state_probs)

# 7. Save Model & Decoded State Dataset
saveRDS(m_hmm, file = "data/processed/lion_hmm_model.rds")
saveRDS(hmm_decoded, file = "data/processed/lion_hmm_decoded.rds")

# 8. Summary Diagnostic
table(hmm_decoded$decoded_state)
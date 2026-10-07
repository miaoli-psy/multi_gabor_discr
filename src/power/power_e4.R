# =====================================================================
# A PRIORI power analysis -- Experiment 4 (adjustment task)
# Determines sample size BEFORE data collection.
#
# Analyses powered (all are participant-level t-tests on summary stats):
#   1. Correlation analysis: paired t-test on Fisher-z (snake vs ladder),
#      per location pair, Holm-corrected across 3 pairs
#   2. Forced-contour slope b: one-sample t vs 0 (snake), and paired
#      snake vs ladder
#   3. Kink K (mean contrast): one-sample t vs 0 (snake)
#
# All assumed effect sizes are A PRIORI -- justify each one in the
# manuscript. Do NOT take them from the data this analysis will plan.
# =====================================================================

library(tidyverse)
library(pwr)

set.seed(45)

N_SIM <- 1000

# =====================================================================
# 1. ASSUMED EFFECTS  -- ### EDIT and justify every value
# =====================================================================

## -- 1a. Correlation analysis -----------------------------------------
# delta_z: snake-vs-ladder difference in Fisher-z of the error
# correlations. Anchor: z = atanh(r), so
#   r .85 vs .65 -> delta_z = atanh(.85) - atanh(.65) = 1.26 - 0.78 = 0.48
#   r .80 vs .65 -> delta_z = 0.32   (a conservative SESOI choice)
DELTA_Z    <- 0.30      ### EDIT -- SESOI for the z difference
SD_Z_DIFF  <- 0.35      ### EDIT -- between-participant SD of the paired
# z difference (from pilot or literature)

## -- 1b. Forced-contour slope b ----------------------------------------
# Full reversal of the middle Gabor = b of -2. Express the assumed
# effect as a FRACTION of a full reversal:
B_SNAKE      <- -2 * 0.25   ### EDIT -- assume 25% of a full reversal
SD_B         <- 0.50        ### EDIT -- between-participant SD of b
B_DIFF       <- 0.40        ### EDIT -- snake-vs-ladder difference in b
SD_B_DIFF    <- 0.50        ### EDIT -- SD of that paired difference

## -- 1c. Kink K ---------------------------------------------------------
K_SNAKE      <- -1.0        ### EDIT -- assumed mean kink in snake (deg)
SD_K         <- 1.5         ### EDIT

## trials per participant x arrangement entering each correlation
N_TRIALS   <- 48            ### EDIT -- your planned trial count

## sample sizes to evaluate
STEPS_N    <- c(10, 15, 20, 25, 30, 40)

## target power and correction
POWER_TARGET <- 0.90
ALPHA        <- 0.05


# =====================================================================
# 2. ANALYTIC POWER CURVES  (pwr package -- fast overview)
# =====================================================================
# dz values; for the correlation analysis the trial-sampling noise
# 1/(n_trials - 3) per arrangement inflates the observed variance:

se_trial2 <- 2 / (N_TRIALS - 3)          # both arrangements contribute

effects <- tibble(
  analysis = c("correlation diff (z)",
               "slope b vs 0 (snake)",
               "slope b snake vs ladder",
               "kink K vs 0 (snake)"),
  dz = c(DELTA_Z / sqrt(SD_Z_DIFF^2 + se_trial2),
         B_SNAKE  / SD_B,
         B_DIFF   / SD_B_DIFF,
         K_SNAKE  / SD_K),
  alpha = c(ALPHA / 3, ALPHA, ALPHA, ALPHA)   # 3 pairs -> Holm ~ Bonferroni
)

analytic <- effects %>%
  rowwise() %>%
  mutate(power = list(map_dbl(STEPS_N, ~ pwr.t.test(
    n = .x, d = dz, sig.level = alpha, type = "paired")$power)),
    N = list(STEPS_N)) %>%
  unnest(c(power, N))

ggplot(analytic, aes(N, power, colour = analysis)) +
  geom_hline(yintercept = POWER_TARGET, linetype = 2) +
  geom_line(linewidth = 1) + geom_point() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "A priori power, Experiment 4 (analytic)",
       y = "power", x = "participants") +
  theme_bw()

# smallest N reaching the target per analysis
analytic %>%
  group_by(analysis) %>%
  summarise(min_N = if (any(power >= POWER_TARGET))
    min(N[power >= POWER_TARGET]) else NA)


# =====================================================================
# 3. MONTE CARLO CHECK  (mirrors the actual pipeline, incl. trial noise)
# =====================================================================
# The analytic solution treats 1/(n-3) as exact; the simulation also
# captures that per-participant correlations are noisy estimates and
# that the three pairs are tested with a multiple-comparison burden.

sim_cor_diff <- function(N, n_tr, delta_z, sd_true, n_sim = N_SIM,
                         alpha = ALPHA / 3) {
  se_z <- 1 / sqrt(n_tr - 3)
  mean(replicate(n_sim, {
    z_true <- rnorm(N, delta_z, sd_true)
    z_obs  <- z_true + rnorm(N, 0, se_z) + rnorm(N, 0, se_z)
    t.test(z_obs)$p.value < alpha
  }))
}

mc <- expand.grid(N = STEPS_N, n_trials = c(24, 48, 96)) %>%        ### EDIT
  rowwise() %>%
  mutate(power = sim_cor_diff(N, n_trials, DELTA_Z, SD_Z_DIFF)) %>%
  ungroup()

ggplot(mc, aes(N, power, colour = factor(n_trials))) +
  geom_hline(yintercept = POWER_TARGET, linetype = 2) +
  geom_line(linewidth = 1) + geom_point() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Correlation difference: participants vs trials",
       colour = "trials per\ncell", y = "power", x = "participants") +
  theme_bw()

print(mc)


# =====================================================================
# 4. DECISION
# =====================================================================
# Base the final N on the WEAKEST powered primary analysis, under the
# most conservative justifiable assumptions. Report: assumed effect
# sizes + their justification, target power, alpha/correction, and the
# resulting N.

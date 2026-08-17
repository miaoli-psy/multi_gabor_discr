library(lme4)
library(mixedpower)
library(pwr)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)


set.seed(45)

# global settings 
STEPS      <- c(5, 10, 15, 20, 25)   # sample sizes 
N_SIM      <- 100 # 100 to test, 1000 to run
CRIT_T     <- 2 
SESOI_MULT <- 0.85  
PILOT_N    <- 6  
MAXC       <- max(1, parallel::detectCores() - 1)

DATA_DIR <- "../../data"            
# setwd("d:/OneDrive/projects/multi_gabor_discr/src/power/")

RESULTS <- list()

# exp1, exp2

CONDS <- c("ladder_radial", "snake_radial", "ladder_tangential", "snake_tangential")

exp1_raw <- readxl::read_excel(file.path(DATA_DIR, "gbr_sc_threshold1.xlsx"))

# staircase to 1 threshould
exp1_thr <- exp1_raw %>%
  filter(full_condition2 %in% CONDS) %>%
  group_by(participant, full_condition2, trials.setsize) %>%
  summarise(threshold   = mean(trials.intensity),
            n_reversals = n(), .groups = "drop")

# pilot
pilot_ids <- sort(unique(exp1_thr$participant))[seq_len(PILOT_N)]

pilot1 <- exp1_thr %>%
  filter(participant %in% pilot_ids) %>%
  mutate(participant     = as.numeric(as.factor(participant)),  
         setsize         = as.numeric(trials.setsize),        
         full_condition2 = factor(full_condition2, levels = CONDS)) %>%
  as.data.frame() 


#threshold n for N=6
nrow(pilot1)

# mean reversal per staircase
round(mean(pilot1$n_reversals), 1)


# fit
fit_e12 <- function(dat, ref) {
  d <- dat
  d$full_condition2 <- relevel(d$full_condition2, ref = ref)
  contrasts(d$full_condition2) <- contr.treatment(4)
  list(model = lmer(threshold ~ setsize * full_condition2 + (1 | participant),
                    data = d, REML = FALSE),
       data  = d)
}

# data-based + one SESOI simulation
simulate_power <- function(model, data, fixed_effects, label) {
  cat("\n  simulating:", label, "\n")
  db <- mixedpower(
    model = model,
    data = data,
    fixed_effects = fixed_effects,
    simvar = "participant",
    steps = STEPS,
    critical_value = CRIT_T,
    n_sim = N_SIM,
    maxCores = MAXC
  )
  db$sim <- "data-based"
  
  b <- fixef(model)
  s <- b
  s[-1] <- b[-1] * SESOI_MULT
  se <- mixedpower(
    model = model,
    data = data,
    fixed_effects = fixed_effects,
    simvar = "participant",
    steps = STEPS,
    critical_value = CRIT_T,
    n_sim = N_SIM,
    SESOI = as.numeric(s),
    databased = FALSE,
    maxCores = MAXC
  )
  se$sim <- paste0("SESOI -", round((1 - SESOI_MULT) * 100), "%")
  
  out <- rbind(db, se)
  out$source <- label
  out
}

e12 <- list()
for (ref in c("ladder_radial", "snake_radial")) {
  f <- fit_e12(pilot1, ref)
  cat("\n--- Exp 1/2 pilot model, reference =", ref, "---\n")
  print(round(summary(f$model)$coefficients, 4))
  e12[[ref]] <- simulate_power(f$model,
                               f$data,
                               c("setsize", "full_condition2"),
                               paste0("Exp1/2 ref=", ref))
}
RESULTS$exp12 <- bind_rows(e12)


# exp3

pilot3 <- pilot1 %>%
  filter(full_condition2 %in% c("ladder_radial", "snake_radial")) %>%
  mutate(arrangement = factor(
    if_else(full_condition2 == "snake_radial", "snake", "ladder"),
    levels = c("ladder", "snake")
  )) %>%
  as.data.frame()



e3 <- list()
for (ref in c("ladder", "snake")) {
  d <- pilot3
  d$arrangement <- relevel(d$arrangement, ref = ref)
  m <- lmer(threshold ~ setsize * arrangement + (1 | participant),
            data = d, REML = FALSE)
  cat("\n--- Exp 3 model, reference =", ref, "---\n")
  print(round(summary(m)$coefficients, 4))
  e3[[ref]] <- simulate_power(m, d, c("setsize", "arrangement"),
                              paste0("Exp3 ref=", ref))
}
RESULTS$exp3 <- bind_rows(e3)


# exp 4

PAIRS      <- c("inner-middle", "middle-outer", "inner-outer")
ALPHA      <- 0.05
ALPHA_CORR <- ALPHA / 3          
DELTA_Z    <- 0.30               
N_TRIALS   <- 225 #trial per condition


angle_dist <- function(a, b) (a - b + 90) %% 180 - 90
fisher_z   <- function(r) atanh(pmin(pmax(r, -0.9999), 0.9999))
safe_cor   <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 3 || sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(cor(x[ok], y[ok]))
}

# participant-level Fisher-z
z_per_participant <- function(d) {
  d %>%
    group_by(participant, arrangement, abs_ori) %>%
    summarise(
      `inner-middle` = safe_cor(err_inner, err_mid),
      `middle-outer` = safe_cor(err_mid, err_outer),
      `inner-outer`  = safe_cor(err_inner, err_outer),
      .groups = "drop"
    ) %>%
    pivot_longer(all_of(PAIRS), names_to = "pair", values_to = "r") %>%
    mutate(z = fisher_z(r)) %>%
    group_by(participant, arrangement, pair) %>%
    summarise(z = mean(z, na.rm = TRUE), .groups = "drop")
}


paired_z_diff <- function(zp) {
  zp %>%
    pivot_wider(names_from = arrangement, values_from = z) %>%
    filter(!is.na(ladder), !is.na(snake)) %>%
    mutate(diff = snake - ladder) %>%
    group_by(pair) %>%
    summarise(
      n = n(),
      mean_diff = mean(diff),
      sd_diff = sd(diff),
      dz = mean(diff) / sd(diff),
      .groups = "drop"
    )
}

# cal assumed SD (0.44)

pilot_files <- list.files(
  file.path(DATA_DIR, "raw_pilot_gabor_adjust_ori"),
  pattern = "\\.csv$",
  full.names = TRUE
)

pilot4 <- map_dfr(pilot_files, function(f) {
  read_csv(f, show_col_types = FALSE, name_repair = "unique_quiet") %>%
    filter(label %in% c("setsize3_r_snake", "setsize3_r_ladder")) %>%
    drop_na(display_resp1, display_resp2, display_resp3, ori) %>%
    transmute(
      participant = str_sub(basename(f), 1, 3),
      arrangement = if_else(str_detect(label, "snake"), "snake", "ladder"),
      abs_ori     = abs(ori),
      s           = if_else(ori < 0, -1, 1),
      # the pilot files predate the stim_location column, so responses
      # are taken in presentation order: 1 and 3 are the two ends,
      # 2 is always the middle element
      err_inner = angle_dist(s * display_resp1, abs_ori),
      err_mid   = angle_dist(s * display_resp2, abs_ori),
      err_outer = angle_dist(s * display_resp3, abs_ori)
    )
})

pilot4_z    <- z_per_participant(pilot4)
pilot4_z
pilot4_diff <- paired_z_diff(pilot4_z)
pilot4_diff



SD_ASSUMED <- 0.35

# power exp4

analytic_for <- function(sd_used, label) {
  dz <- DELTA_Z / sqrt(sd_used^2 + 2 / (N_TRIALS - 3))
  tibble(N = STEPS) %>%
    rowwise() %>%
    mutate(
      assumption = label,
      sd_used = sd_used,
      implied_dz = dz,
      power_corrected   = pwr.t.test(
        n = N,
        d = dz,
        sig.level = ALPHA_CORR,
        type = "paired"
      )$power,
      power_uncorrected = pwr.t.test(
        n = N,
        d = dz,
        sig.level = ALPHA,
        type = "paired"
      )$power
    ) %>%
    ungroup()
}

exp4_analytic <- bind_rows(
  analytic_for(SD_ASSUMED, "SD = 0.35 (as originally assumed)"),
  analytic_for(SD_PILOT,   "SD from pilot, computed as in the analysis"))

exp4_analytic




# smallest delta_z the design can detect. to report if without assusemd SD

exp4_detectable <- tibble(N = STEPS) %>%
  rowwise() %>%
  mutate(
    delta_z_80 = pwr.t.test(
      n = N,
      power = .80,
      sig.level = ALPHA_CORR,
      type = "paired"
    )$d *
      sqrt(SD_PILOT^2 + 2 / (N_TRIALS - 3)),
    delta_z_90 = pwr.t.test(
      n = N,
      power = .90,
      sig.level = ALPHA_CORR,
      type = "paired"
    )$d *
      sqrt(SD_PILOT^2 + 2 / (N_TRIALS - 3))
  ) %>%
  ungroup()

exp4_detectable

# #Monte Carlo check 
# sim_cor_diff <- function(N, n_trials, delta_z, sd_true,
#                          n_sim = N_SIM, alpha = ALPHA_CORR) {
#   se_z <- 1 / sqrt(n_trials - 3)
#   mean(replicate(n_sim, {
#     z_true <- rnorm(N, delta_z, sd_true)
#     z_obs  <- z_true + rnorm(N, 0, se_z) + rnorm(N, 0, se_z)   # both arrangements
#     t.test(z_obs)$p.value < alpha
#   }))
# }
# 
# exp4_mc <- expand.grid(N = STEPS, n_trials = c(75, 225)) %>%
#   rowwise() %>%
#   mutate(power = sim_cor_diff(N, n_trials, DELTA_Z, SD_ASSUMED)) %>%
#   ungroup()


#sensitivity for models of exp4

exp4_sens <- tibble(N = STEPS) %>%
  rowwise() %>%
  mutate(
    dz_80_corrected = pwr.t.test(
      n = N,
      power = .80,
      sig.level = ALPHA_CORR,
      type = "paired"
    )$d,
    dz_90_corrected = pwr.t.test(
      n = N,
      power = .90,
      sig.level = ALPHA_CORR,
      type = "paired"
    )$d,
    dz_80_uncorr    = pwr.t.test(
      n = N,
      power = .80,
      sig.level = ALPHA,
      type = "paired"
    )$d,
    dz_90_uncorr    = pwr.t.test(
      n = N,
      power = .90,
      sig.level = ALPHA,
      type = "paired"
    )$d
  ) %>%
  ungroup()


exp4_obs <- read_csv(file.path(DATA_DIR, "gabor_adjst_ori_alldata.csv"),
                     show_col_types = FALSE) %>%
  filter(label %in% c("setsize3_r_ladder", "setsize3_r_snake")) %>%
  drop_na(inner_resp, midd_resp, outer_resp) %>%
  mutate(
    arrangement = if_else(str_detect(label, "snake"), "snake", "ladder"),
    s           = if_else(ori < 0, -1, 1),
    err_inner   = angle_dist(s * inner_resp, abs_ori),
    err_mid     = angle_dist(s * midd_resp, abs_ori),
    err_outer   = angle_dist(s * outer_resp, abs_ori)
  ) %>%
  z_per_participant() %>%
  paired_z_diff()


RESULTS$exp4 <- list(pilot_sd = pilot4_diff, analytic = exp4_analytic,
                     detectable = exp4_detectable, monte_carlo = exp4_mc,
                     sensitivity = exp4_sens, observed = exp4_obs)
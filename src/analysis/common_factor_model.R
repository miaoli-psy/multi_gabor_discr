# Exp 4: common-factor model of adjustment errors

library(tidyverse)
library(gghalves)
library(ggplot2)

setwd("d:/OneDrive/projects/multi_gabor_discr/src/analysis/")

PAL <- c(ladder = "#F28522", snake = "#674EA7")


# helpers
# shortest signed angular difference [-90, 90)
angle_dist <- function(a, b) (a - b + 90) %% 180 - 90

# data

data_exp4 <- read_csv("../../data/gabor_adjst_ori_alldata.csv")

data <- data_exp4 %>%
  dplyr::select(participant, label, ori, abs_ori,
                inner_resp, midd_resp, outer_resp) %>%
  filter(label %in% c("setsize3_r_ladder", "setsize3_r_snake")) %>%
  drop_na(inner_resp, midd_resp, outer_resp) %>%
  mutate(
    arr = if_else(str_detect(label, "snake"), "snake", "ladder"),
    s   = if_else(ori < 0, -1, 1),
    # fold: reflect CCW trials, so -11 reported for -10 equals +11 for +10
    err_inner = angle_dist(s * inner_resp, abs_ori),
    err_mid   = angle_dist(s * midd_resp,  abs_ori),
    err_outer = angle_dist(s * outer_resp, abs_ori)
  )


# center within participant x arrangement x orientation -> removes mu
# here, any systematic bias (participant-specific or orientation specific is removed)
# thus, the fitted covariance structure reflects pure trial to trial variability around each
# participant's own bias

data_c <- data %>%
  group_by(participant, arr, abs_ori) %>%
  mutate(across(c(err_inner, err_mid, err_outer),
                ~ .x - mean(.x, na.rm = TRUE))) %>%
  ungroup()


# 3-column error matrix

err_matrix <- function(df) {
  as.matrix(df[, c("err_inner", "err_mid", "err_outer")])
}


# negative log-likelihood, no mean term (data are centred)

nll_cf <- function(par, X) {
  lam <- par[1:3]
  psi <- exp(par[4:6])   # keep local variances positive

  S  <- lam %*% t(lam) + diag(psi)
  ld <- determinant(S, logarithm = TRUE)
  if (ld$sign <= 0) return(1e10)

  0.5 * (nrow(X) * as.numeric(ld$modulus) + sum((X %*% solve(S)) * X))
}


# fit

fit_cf <- function(X, n_starts = 8) {
  X <- X[complete.cases(X), , drop = FALSE]

  if (nrow(X) < 15) {
    return(list(index = NA_real_, shared = rep(NA_real_, 3),
                lam = rep(NA_real_, 3), psi = rep(NA_real_, 3),
                n = nrow(X), conv = NA_integer_))
  }

  v  <- pmax(apply(X, 2, function(z) mean(z^2)), 1e-3)
  p0 <- c(sqrt(v) * 0.6, log(v * 0.5 + 1))

  best <- NULL
  for (i in seq_len(n_starts)) {
    init <- p0 + rnorm(6, 0, 0.5)
    fit  <- optim(init, nll_cf, X = X, method = "Nelder-Mead",
                  control = list(maxit = 20000, reltol = 1e-10))
    if (is.null(best) || fit$value < best$value) best <- fit
  }

  lam <- best$par[1:3]
  psi <- exp(best$par[4:6])

  list(index  = mean(lam^2 / (lam^2 + psi)),
       shared = lam^2 / (lam^2 + psi),
       lam    = lam,          # raw, sign preserved
       psi    = psi,
       n      = nrow(X),
       conv   = best$convergence,
       value  = best$value)
}


# fit per participant per arrangement

set.seed(42)

per_pp <- data_c %>%
  group_by(participant, arr) %>%
  group_modify(~{
    f <- fit_cf(err_matrix(.x))
    tibble(
      index        = f$index,
      shared_inner = f$shared[1],
      shared_mid   = f$shared[2],
      shared_outer = f$shared[3],
      lam_inner    = f$lam[1],
      lam_mid      = f$lam[2],
      lam_outer    = f$lam[3],
      psi_inner    = f$psi[1],
      psi_mid      = f$psi[2],
      psi_outer    = f$psi[3],
      n            = f$n,
      conv         = f$conv
    )
  }) %>%
  ungroup()

# all fits should converge (conv == 0)
table(per_pp$conv)

# loadings should share a sign within a fit; mixed signs need looking at
per_pp %>%
  mutate(mixed_sign = !(sign(lam_inner) == sign(lam_mid) &
                          sign(lam_mid) == sign(lam_outer))) %>%
  count(arr, mixed_sign)


# shared variance index

idx_summary <- per_pp %>%
  group_by(arr) %>%
  summarise(n        = n(),
            index_M  = mean(index, na.rm = TRUE),
            index_SD = sd(index, na.rm = TRUE),
            .groups  = "drop")

idx_summary

per_pp_wide <- per_pp %>%
  dplyr::select(participant, arr, index) %>%
  pivot_wider(names_from = arr, values_from = index)

tt <- t.test(per_pp_wide$ladder, per_pp_wide$snake, paired = TRUE)
tt

diff <- per_pp_wide$ladder - per_pp_wide$snake
dz   <- mean(diff) / sd(diff)
dz


# per-location shared fractions, and check there is no inner-outer gradient

per_pp %>%
  group_by(arr) %>%
  summarise(inner = mean(shared_inner, na.rm = TRUE),
            mid   = mean(shared_mid,   na.rm = TRUE),
            outer = mean(shared_outer, na.rm = TRUE),
            .groups = "drop")

snake_pl <- per_pp %>% filter(arr == "snake")
t.test(snake_pl$shared_inner, snake_pl$shared_outer, paired = TRUE)

ladder_pl <- per_pp %>% filter(arr == "ladder")
t.test(ladder_pl$shared_inner, ladder_pl$shared_outer, paired = TRUE)


# model comparison against an independence model (no common factor)
# common factor: 3 lambda + 3 psi = 6 params
# independence : 3 psi           = 3 params
# full nll WITH the constant term so AIC/BIC are on the right scale

nll_full <- function(S, X) {
  ld <- determinant(S, logarithm = TRUE)
  0.5 * (nrow(X) * as.numeric(ld$modulus) +
           sum((X %*% solve(S)) * X) +
           nrow(X) * ncol(X) * log(2 * pi))
}

fit_independent <- function(X) {
  X   <- X[complete.cases(X), , drop = FALSE]
  psi <- apply(X, 2, function(z) mean(z^2))   # MLE, data already centred
  nll <- nll_full(diag(psi), X)
  k   <- 3
  list(nll = nll, k = k, aic = 2*k + 2*nll, bic = k*log(nrow(X)) + 2*nll)
}

fit_cf_ic <- function(X) {
  f   <- fit_cf(X)
  S   <- f$lam %*% t(f$lam) + diag(f$psi)   # raw lambda, not abs()
  nll <- nll_full(S, X)
  k   <- 6
  list(nll = nll, k = k, aic = 2*k + 2*nll, bic = k*log(nrow(X)) + 2*nll)
}

set.seed(42)

model_compare <- map_dfr(c("ladder", "snake"), function(a) {
  X   <- err_matrix(filter(data_c, arr == a))
  ind <- fit_independent(X)
  cf  <- fit_cf_ic(X)
  tibble(arr = a,
         AIC_independent = ind$aic, AIC_factor = cf$aic,
         BIC_independent = ind$bic, BIC_factor = cf$bic,
         dAIC = ind$aic - cf$aic,     # positive => common-factor model wins
         dBIC = ind$bic - cf$bic)
})

model_compare


# plot


idx_across_pp <- per_pp %>%
  group_by(arr) %>%
  summarise(avg_index = mean(index, na.rm = TRUE), .groups = "drop")

#fig4a
p_idx <- ggplot(per_pp, aes(x = arr, y = index, fill = arr)) +
  geom_half_violin(
    side = "l",
    alpha = 0.3,
    width = 1,
    color = "white",
    trim = FALSE
  ) +
  geom_line(aes(group = participant),
            color = "grey",
            linewidth = 0.4) +
  geom_point(aes(color = arr), size = 2.2, alpha = 0.5) +
  geom_point(data = idx_across_pp,
             aes(x = arr, y = avg_index, color = arr),
             size = 6) +
  scale_fill_manual(values  = PAL) +
  scale_color_manual(values = PAL) +
  scale_x_discrete(labels = c(ladder = "Ladder", snake = "Snake")) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = NULL, y = "Shared variance index") +
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    legend.position = "none",
    strip.text = element_text(size = 14, face = "bold"),
    panel.spacing = unit(1.0, "lines")
  )

p_idx

# ggsave("p_idx.svg", p_idx, width = 3, height = 4, units = "in")


# observed vs model-implied correlations
#
# CONVERGENCE CHECK ONLY, not evidence of fit. The model has 6 parameters and
# 6 sufficient statistics (3 variances, 3 covariances), so it is exactly
# identified (df = 0) and the two matrices must agree. A close match confirms
# the optimiser found the saturated solution, nothing more. Fit evidence comes
# from model_compare above. -> supplementary figureS

# observed correlation matrix

get_empirical_cor <- function(df) {
  X <- df %>%
    dplyr::select(err_inner, err_mid, err_outer)

  cor_mat <- cor(X, use = "complete.obs")

  as.data.frame(as.table(cor_mat)) %>%
    rename(location_y = Var1,
           location_x = Var2,
           r          = Freq)
}


# pooled fit per arrangement, keep loadings + psi
# (pooled across participants: this figure is about structure, not stats)

set.seed(42)

fit_ladder <- fit_cf(err_matrix(filter(data_c, arr == "ladder")))
fit_snake  <- fit_cf(err_matrix(filter(data_c, arr == "snake")))


# fitted loadings + psi -> model correlation matrix
# cov = lambda lambda' + diag(psi), then scale to correlation

model_cor_from_fit <- function(f) {
  lam <- f$lam        # raw lambda, sign preserved
  psi <- f$psi
  S   <- lam %*% t(lam) + diag(psi)
  D   <- sqrt(diag(S))
  Cmod <- S / (D %o% D)
  dimnames(Cmod) <- list(
    c("err_inner", "err_mid", "err_outer"),
    c("err_inner", "err_mid", "err_outer")
  )
  as.data.frame(as.table(Cmod)) %>%
    rename(location_y = Var1, location_x = Var2, r = Freq)
}

model_cor_df <- bind_rows(
  model_cor_from_fit(fit_ladder) %>% mutate(arr = "ladder"),
  model_cor_from_fit(fit_snake)  %>% mutate(arr = "snake")
) %>%
  mutate(source = "Model")

obs_cor_df <- data_c %>%
  group_by(arr) %>%
  group_modify(~ get_empirical_cor(.x)) %>%
  ungroup() %>%
  mutate(source = "Observed")


# halve the matrix

both_cor_df <- bind_rows(obs_cor_df, model_cor_df) %>%
  mutate(
    arr = factor(arr, levels = c("ladder", "snake"),
                 labels = c("Ladder", "Snake")),
    source = factor(source, levels = c("Observed", "Model")),
    location_x = factor(location_x,
                        levels = c("err_inner", "err_mid", "err_outer"),
                        labels = c("Inner", "Middle", "Outer")),
    location_y = factor(location_y,
                        levels = c("err_inner", "err_mid", "err_outer"),
                        labels = c("Inner", "Middle", "Outer")),
    r_label = sprintf("%.3f", r)
  ) %>%
  # keeps one half of the matrix plus the diagonal
  filter(as.numeric(location_x) <= as.numeric(location_y))


# biggest observed-vs-model discrepancy: should be ~0 if the fit converged

both_cor_df %>%
  dplyr::select(arr, location_x, location_y, source, r) %>%
  pivot_wider(names_from = source, values_from = r) %>%
  mutate(abs_diff = abs(Observed - Model)) %>%
  arrange(desc(abs_diff))


p_cor_compare <- ggplot(both_cor_df,
                        aes(x = location_x, y = location_y, fill = r)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = r_label), size = 4, fontface = "bold") +
  facet_grid(source ~ arr) +
  scale_fill_gradient2(
    low = "#3B4CC0",
    mid = "white",
    high = "#B40426",
    midpoint = 0,
    limits = c(-1.0001, 1.0001),  # keeps the 1s from going grey
    name = "r"
  ) +
  coord_fixed() +
  labs(x = NULL, y = NULL,
       title = "Observed vs model-implied correlations") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 11, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  )

p_cor_compare

# ggsave("fig_cor_compare.svg", p_cor_compare, width = 7, height = 6, units = "in")
